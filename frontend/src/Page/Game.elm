module Page.Game exposing (Model, Msg, init, subscriptions, update, view)

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
    | HandleGetGameResp (Result Http.Error BE.DbGameState)


type alias Model =
    { gameId : BE.GameId
    , gameState : RemoteData String BE.DbGameState
    , lastUpdated : Maybe Time.Posix
    , newChatLine : String
    , chatLines : List BE.ChatLine
    , chatListError : Maybe String
    , validationIssues : List String
    , newLineSubmission : RemoteData String ()
    }


type alias PageMsg =
    Page.SubMsg Msg


init : Nav.Key -> Session.User -> BE.GameId -> ( Model, Cmd PageMsg )
init key user gId =
    ( { gameId = gId
      , gameState = RemoteData.Loading
      , newChatLine = ""
      , lastUpdated = Nothing
      , chatLines = []
      , chatListError = Nothing
      , validationIssues = []
      , newLineSubmission = RemoteData.NotAsked
      }
    , Cmd.batch
        [ BE.getApiGameByGameId user.token gId (Page.wrapChildMsg HandleGetGameResp)
        , Task.perform (Page.wrapChildMsg Tick) Time.now
        ]
    )


subscriptions : Session.User -> Model -> Sub PageMsg
subscriptions _ _ =
    Time.every 5000 (Page.wrapChildMsg Tick)


update : Nav.Key -> Session.User -> Msg -> Model -> ( Model, Cmd PageMsg )
update key user msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetNewLine l ->
            ( { model | newChatLine = l }, Cmd.none )

        SubmitNewLine ->
            case validateNewChatLine user model of
                Ok newChatLine ->
                    ( { model | validationIssues = [], newLineSubmission = RemoteData.Loading }
                    , BE.postApiGameByGameIdChat user.token model.gameId newChatLine (Page.wrapChildMsg HandleNewLineResp)
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
            , BE.getApiGameByGameIdChat
                user.token
                model.gameId
                (model.chatLines |> List.map (.chatLineTime >> Time.posixToMillis) |> List.maximum)
                (Page.wrapChildMsg HandleListResp)
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

        HandleGetGameResp r ->
            let
                remoteData =
                    RemoteData.fromResult r
                        |> RemoteData.mapError Utils.httpErrorToStr
            in
            ( { model | gameState = remoteData }, Cmd.none )


jumpToChatBottom : Cmd PageMsg
jumpToChatBottom =
    Dom.getViewportOf "chatbox"
        |> Task.andThen (\info -> Dom.setViewportOf "chatbox" 0 info.scene.height)
        |> Task.attempt (\_ -> Page.ChildMsg NoOp)



-- elm-verify is a much better way of doing this. But this is our only validation.
-- Come back to this later.


validateNewChatLine : Session.User -> Model -> Result.Result (List String) String
validateNewChatLine user model =
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


view : Session.User -> Model -> Browser.Document Msg
view user model =
    { title = "Dissidence - Game"
    , body =
        [ H.div []
            [ H.div [ HA.class "chatbox-container" ]
                [ H.h1 [] [ H.text "Game ", H.text (String.fromInt model.gameId) ]
                , H.div [ HA.id "chatbox", HA.class "chatbox" ] (List.map chatLineView model.chatLines)
                , H.form [ HE.onSubmit SubmitNewLine ]
                    [ H.ul []
                        [ H.li [ HA.class "chat-message" ]
                            [ H.input
                                [ HA.placeholder "type a chat message"
                                , HE.onInput SetNewLine
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
                [ case model.gameState of
                    RemoteData.NotAsked ->
                        H.text "NOT ASKED"

                    RemoteData.Loading ->
                        H.text "LOADING"

                    RemoteData.Failure s ->
                        H.text ("ERROR: " ++ s)

                    RemoteData.Success gs ->
                        case gs.dbGameState of
                            BE.WaitingForPlayers o ps ->
                                waitingForPlayers user o ps

                            _ ->
                                H.text "IMPLEMENT ME"
                ]
            ]
        ]
    }


waitingForPlayers : Session.User -> BE.PlayerId -> List BE.PlayerId -> H.Html Msg
waitingForPlayers user owner players =
    H.div [ HA.class "players-waiting" ]
        [ H.h2 [] [ H.text "Players Waiting" ]
        , H.ul []
            (H.li [] [ H.text owner, H.text " (Owner)" ]
                :: List.map (H.text >> List.singleton >> H.li []) players
            )
        , if List.length players >= 5 && owner == user.username then
            H.button [] [ H.text "Start Game" ]

          else
            H.text ""
        ]


chatWarnings : NEL.Nonempty String -> H.Html Msg
chatWarnings errors =
    H.li [ HA.class "chat-warnings" ] [ H.ul [ HA.class "warn" ] (List.map (\em -> H.li [] [ H.text em ]) (NEL.toList errors)) ]


chatLineView : BE.ChatLine -> H.Html Msg
chatLineView cl =
    H.p []
        [ H.b [] [ H.text cl.chatLineUsername, H.text "> " ]
        , H.text cl.chatLineText
        ]
