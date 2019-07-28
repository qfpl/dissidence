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
    | StartGame
    | HandleNewLineResp (Result Http.Error ())
    | HandleEventsResp (Result Http.Error (List BE.GameEvent))
    | HandleGetGameResp (Result Http.Error BE.DbGameState)
    | HandleNewEventResp (Result Http.Error ())


type alias Model =
    { gameId : BE.GameId
    , gameState : RemoteData String BE.DbGameState
    , lastUpdated : Maybe Time.Posix
    , newChatLine : String
    , validationIssues : List String
    , newLineSubmission : RemoteData String ()
    , gameEvents : List BE.GameEvent
    , eventsListError : Maybe String
    }


type alias PageMsg =
    Page.SubMsg Msg


init : Nav.Key -> Session.Player -> BE.GameId -> ( Model, Cmd PageMsg )
init key player gId =
    ( { gameId = gId
      , gameState = RemoteData.Loading
      , newChatLine = ""
      , lastUpdated = Nothing
      , validationIssues = []
      , newLineSubmission = RemoteData.NotAsked
      , gameEvents = []
      , eventsListError = Nothing
      }
    , Cmd.batch
        [ BE.getApiGamesByGameId player.token gId (Page.wrapChildMsg HandleGetGameResp)
        , Task.perform (Page.wrapChildMsg Tick) Time.now
        ]
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
                    , BE.postApiGamesByGameIdEvents player.token model.gameId (BE.NewGameEventChat newChatLine) (Page.wrapChildMsg HandleNewLineResp)
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
                [ BE.getApiGamesByGameIdEvents
                    player.token
                    model.gameId
                    (model.gameEvents |> List.map (.gameEventTime >> Time.posixToMillis) |> List.maximum)
                    (Page.wrapChildMsg HandleEventsResp)
                , BE.getApiGamesByGameId player.token model.gameId (Page.wrapChildMsg HandleGetGameResp)
                ]
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

        HandleEventsResp (Err e) ->
            ( { model | eventsListError = Just (Utils.httpErrorToStr e) }, Cmd.none )

        HandleEventsResp (Ok l) ->
            ( { model
                | eventsListError = Nothing
                , gameEvents =
                    if model.gameEvents == [] then
                        l

                    else
                        model.gameEvents ++ l
              }
            , jumpToChatBottom
            )

        HandleNewEventResp _ ->
            ( model, Cmd.none )

        StartGame ->
            -- TODO: Some kind of in progress / error display feedback
            ( model, BE.postApiGamesByGameIdEvents player.token model.gameId (BE.NewGameEventInput BE.StartGame) (Page.wrapChildMsg HandleNewEventResp) )


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
    { title = "Dissidence - Game"
    , body =
        [ Page.logoutView player
        , H.div [ HA.class "game-page" ]
            [ H.div [ HA.class "game-state" ]
                [ H.h1 [] [ H.text "Game ", H.text (String.fromInt model.gameId) ]
                , case model.gameState of
                    RemoteData.NotAsked ->
                        H.text "NOT ASKED"

                    RemoteData.Loading ->
                        H.text "LOADING"

                    RemoteData.Failure s ->
                        H.text ("ERROR: " ++ s)

                    RemoteData.Success gs ->
                        case gs.dbGameState of
                            BE.WaitingForPlayers o ps ->
                                waitingForPlayers player o ps

                            _ ->
                                H.text "IMPLEMENT ME"
                ]
            , H.div [ HA.class "chatbox-container" ]
                [ H.h2 [] [ H.text "Game Chat" ]
                , H.div [ HA.id "chatbox", HA.class "chatbox" ] (List.map chatLineView model.gameEvents)
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
            ]
        ]
    }


waitingForPlayers : Session.Player -> BE.PlayerId -> List BE.PlayerId -> H.Html PageMsg
waitingForPlayers player owner players =
    H.div [ HA.class "players-waiting" ]
        [ H.h2 [] [ H.text "Players Waiting" ]
        , H.ul []
            (H.li [] [ H.text owner, H.text " (Owner)" ]
                :: List.map (H.text >> List.singleton >> H.li []) players
            )
        , if List.length players >= 4 && owner == player.playerId then
            H.button [ HE.onClick (Page.ChildMsg StartGame) ] [ H.text "Start Game" ]

          else
            H.text ""
        ]


chatWarnings : NEL.Nonempty String -> H.Html PageMsg
chatWarnings errors =
    H.li [ HA.class "chat-warnings" ] [ H.ul [ HA.class "warn" ] (List.map (\em -> H.li [] [ H.text em ]) (NEL.toList errors)) ]


chatLineView : BE.GameEvent -> H.Html PageMsg
chatLineView ge =
    case ge.gameEventData of
        BE.GameEventChat cl ->
            H.p []
                [ H.b [] [ H.text cl.chatLinePlayerId, H.text "> " ]
                , H.text cl.chatLineText
                ]

        BE.GameEventOutput oe ->
            H.p [] [ H.text "OUTPUTEVENT" ]
