module Page.Lobby exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Generated.Api as BE
import Html as H
import Html.Attributes as HA
import Html.Attributes.Aria as HAA
import Html.Events as HE
import Http
import List.Nonempty as NEL
import Page
import RemoteData exposing (RemoteData)
import Result
import Route
import Session
import Task
import Time
import Utils exposing (disabledIfLoading, maybe, maybeToList, remoteDataError)


type Msg
    = NoOp
    | SetNewLine String
    | Tick Time.Posix
    | SubmitNewLine
    | HandleNewLineResp (Result Http.Error ())
    | HandleListResp (Result Http.Error (List BE.ChatLine))
    | MakeNewGame
    | HandleNewGameResp (Result Http.Error BE.GameId)
    | HandleJoinableResp (Result Http.Error (List BE.JoinableGame))
    | HandleJoinResp BE.GameId (Result Http.Error ())
    | JoinGame BE.GameId


type alias Model =
    { lastUpdated : Maybe Time.Posix
    , newChatLine : String
    , chatLines : List BE.ChatLine
    , chatListError : Maybe String
    , validationIssues : List String
    , newLineSubmission : RemoteData String ()
    , newGameSubmission : RemoteData String BE.GameId
    , joinableGames : List BE.JoinableGame
    , joinGameSubmission : RemoteData String BE.GameId
    }


type alias PageMsg =
    Page.SubMsg Msg


init : Nav.Key -> Session.Player -> ( Model, Cmd PageMsg )
init key player =
    ( { newChatLine = ""
      , lastUpdated = Nothing
      , chatLines = []
      , chatListError = Nothing
      , validationIssues = []
      , newLineSubmission = RemoteData.NotAsked
      , newGameSubmission = RemoteData.NotAsked
      , joinableGames = []
      , joinGameSubmission = RemoteData.NotAsked
      }
    , Task.perform (Page.wrapChildMsg Tick) Time.now
    )


subscriptions : Session.Player -> Model -> Sub PageMsg
subscriptions _ _ =
    Time.every 5000 (Page.wrapChildMsg Tick)


update : Nav.Key -> Session.Player -> Msg -> Model -> ( Model, Cmd PageMsg )
update key player msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetNewLine l ->
            ( { model | newChatLine = l }, Cmd.none )

        SubmitNewLine ->
            case validateNewChatLine player model of
                Ok newChatLine ->
                    ( { model | validationIssues = [], newLineSubmission = RemoteData.Loading }
                    , BE.postApiLobby player.token newChatLine (Page.wrapChildMsg HandleNewLineResp)
                    )

                Err problems ->
                    ( { model
                        | newLineSubmission = RemoteData.NotAsked
                        , validationIssues = problems
                      }
                    , Cmd.none
                    )

        Tick time ->
            ( model
            , Cmd.batch
                [ BE.getApiLobby
                    player.token
                    (model.chatLines |> List.map (.chatLineTime >> Time.posixToMillis) |> List.maximum)
                    (Page.wrapChildMsg HandleListResp)
                , BE.getApiGamesJoinable player.token (Page.wrapChildMsg HandleJoinableResp)
                ]
            )

        HandleListResp (Err e) ->
            ( { model | chatListError = Just (Utils.httpErrorToStr e) }, Cmd.none )

        HandleListResp (Ok l) ->
            ( { model
                | chatListError = Nothing
                , chatLines =
                    if model.chatLines == [] then
                        l

                    else
                        model.chatLines ++ l
              }
            , jumpToChatBottom
            )

        HandleNewLineResp r ->
            let
                remoteData =
                    RemoteData.fromResult r
                        |> RemoteData.mapError Utils.httpErrorToStr
            in
            ( { model | newLineSubmission = remoteData, newChatLine = "" }
            , RemoteData.unwrap
                Cmd.none
                (\us -> Task.perform (Page.wrapChildMsg Tick) Time.now)
                remoteData
            )

        MakeNewGame ->
            ( { model | newGameSubmission = RemoteData.Loading }
            , BE.postApiGames player.token (Page.wrapChildMsg HandleNewGameResp)
            )

        HandleNewGameResp r ->
            let
                remoteData =
                    RemoteData.fromResult r
                        |> RemoteData.mapError Utils.httpErrorToStr
            in
            ( { model | newGameSubmission = remoteData }
            , RemoteData.unwrap
                Cmd.none
                (\gId -> Route.pushRoute key (Route.Game gId))
                remoteData
            )

        HandleJoinableResp (Err e) ->
            ( model, Cmd.none )

        HandleJoinableResp (Ok gs) ->
            ( { model | joinableGames = gs }, Cmd.none )

        JoinGame gId ->
            ( { model | joinGameSubmission = RemoteData.Loading }
            , BE.postApiGamesByGameIdEvents player.token gId (BE.NewGameEventInput BE.AddPlayer) (Page.wrapChildMsg (HandleJoinResp gId))
            )

        HandleJoinResp gId r ->
            let
                remoteData =
                    RemoteData.fromResult r
                        |> RemoteData.mapBoth (always gId) Utils.httpErrorToStr
            in
            ( { model | joinGameSubmission = remoteData }
            , RemoteData.unwrap
                Cmd.none
                (\_ -> Route.pushRoute key (Route.Game gId))
                remoteData
            )


jumpToChatBottom : Cmd PageMsg
jumpToChatBottom =
    Dom.getViewportOf "chatbox"
        |> Task.andThen (\info -> Dom.setViewportOf "chatbox" 0 info.scene.height)
        |> Task.attempt (\_ -> Page.ChildMsg NoOp)



-- elm-verify is a much better way of doing this. But this is our only validation.
-- Come back to this later.


validateNewChatLine : Session.Player -> Model -> Result.Result (List String) String
validateNewChatLine player model =
    let
        trimmedLine =
            String.trim model.newChatLine

        newChatLineError =
            if trimmedLine == "" then
                [ "Message cannot be blank" ]

            else
                []

        allErrs =
            List.concat [ newChatLineError ]
    in
    if allErrs == [] then
        Result.Ok trimmedLine

    else
        Result.Err allErrs


view : Session.Player -> Model -> Browser.Document PageMsg
view player model =
    { title = "Dissidence - Lobby"
    , body =
        [ Page.logoutView player
        , H.div [ HA.class "chatbox-container" ]
            [ H.h1 [] [ H.text "Lobby" ]
            , H.div [ HA.id "chatbox", HA.class "chatbox" ] (List.map chatLineView model.chatLines)
            , H.form [ HE.onSubmit (Page.ChildMsg SubmitNewLine) ]
                [ H.ul []
                    [ H.li [ HA.class "chat-message" ]
                        [ H.input
                            [ HA.placeholder "type a chat message"
                            , HE.onInput (Page.wrapChildMsg SetNewLine)
                            , HA.value model.newChatLine
                            , HA.class "chat-message-input"
                            , HAA.ariaLabel "Enter Chat Message"
                            ]
                            []
                        ]
                    , H.li []
                        [ H.button
                            [ HA.class "btn primary", disabledIfLoading model.newLineSubmission ]
                            [ H.text "send" ]
                        ]
                    , let
                        errorsMay =
                            NEL.fromList (maybeToList (remoteDataError model.newLineSubmission) ++ model.validationIssues)
                      in
                      Utils.maybe (H.text "") chatWarnings errorsMay
                    ]
                ]
            ]
        , H.div []
            [ H.ul [] (List.map joinableGame model.joinableGames)
            , H.button [ HE.onClick (Page.ChildMsg MakeNewGame) ] [ H.text "New Game" ]
            ]
        ]
    }


joinableGame : BE.JoinableGame -> H.Html PageMsg
joinableGame g =
    H.li []
        [ H.text ("Game " ++ String.fromInt g.joinableGameId ++ " (" ++ String.fromInt g.playerCount ++ "/10)")
        , H.button [ HE.onClick (Page.ChildMsg (JoinGame g.joinableGameId)) ] [ H.text "Join" ]
        ]


chatWarnings : NEL.Nonempty String -> H.Html PageMsg
chatWarnings errors =
    H.li [ HA.class "chat-warnings" ] [ H.ul [ HA.class "warn" ] (List.map (\em -> H.li [] [ H.text em ]) (NEL.toList errors)) ]


chatLineView : BE.ChatLine -> H.Html PageMsg
chatLineView cl =
    H.p []
        [ H.b [] [ H.text cl.chatLinePlayerId, H.text "> " ]
        , H.text cl.chatLineText
        ]
