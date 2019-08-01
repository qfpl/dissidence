module Page.Game exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Dict exposing (Dict)
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
import Set exposing (Set)
import Task
import Time
import Utils exposing (disabledIfLoading, maybe, maybeToList, remoteDataError)


type Msg
    = NoOp
    | SetNewLine String
    | Tick Time.Posix
    | SubmitNewLine
    | StartGame
    | ConfirmRole
    | HandleNewLineResp (Result Http.Error ())
    | HandleEventsResp (Result Http.Error (List BE.ViewStateEvent))
    | HandleGetGameResp (Result Http.Error BE.DbViewState)
    | HandleNewEventResp (Result Http.Error ())
    | ToggleTeamMemberProposal BE.PlayerId
    | ConfirmTeamProposal
    | VoteOnProject Bool
    | VoteOnTeam Bool
    | FirePlayer BE.PlayerId


type alias RoundsViewModel =
    { teamSelected : Set BE.PlayerId }


type alias Model =
    { gameId : BE.GameId
    , gameState : RemoteData String BE.DbViewState
    , lastUpdated : Maybe Time.Posix
    , newChatLine : String
    , validationIssues : List String
    , newLineSubmission : RemoteData String ()
    , gameEvents : List BE.ViewStateEvent
    , eventsListError : Maybe String
    , roundsVm : RoundsViewModel
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
      , roundsVm = { teamSelected = Set.empty }
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
                    , BE.postApiGamesByGameIdEvents player.token model.gameId (BE.NewViewStateEventChat newChatLine) (Page.wrapChildMsg HandleNewLineResp)
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
                    (model.gameEvents |> List.map (.viewStateEventTime >> Time.posixToMillis) |> List.maximum)
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
            inputEvent player model BE.StartGame

        ConfirmRole ->
            inputEvent player model BE.ConfirmOk

        ToggleTeamMemberProposal pId ->
            let
                oldRoundsVm =
                    model.roundsVm

                newRoundsViewModel =
                    { oldRoundsVm
                        | teamSelected =
                            if Set.member pId model.roundsVm.teamSelected then
                                Set.remove pId oldRoundsVm.teamSelected

                            else
                                Set.insert pId oldRoundsVm.teamSelected
                    }
            in
            ( { model | roundsVm = newRoundsViewModel }, Cmd.none )

        ConfirmTeamProposal ->
            inputEvent player model (BE.ProposeTeam (Set.toList model.roundsVm.teamSelected))

        VoteOnTeam b ->
            inputEvent player model (BE.VoteOnTeam b)

        VoteOnProject b ->
            inputEvent player model (BE.VoteOnProject b)

        FirePlayer pId ->
            inputEvent player model (BE.FirePlayer pId)


inputEvent : Session.Player -> Model -> BE.GameStateInputEvent -> ( Model, Cmd PageMsg )
inputEvent player model iE =
    ( model, BE.postApiGamesByGameIdEvents player.token model.gameId (BE.NewViewStateEventInput iE) (Page.wrapChildMsg HandleNewEventResp) )


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
                        case gs.dbViewState of
                            BE.WaitingForPlayers o ps ->
                                waitingForPlayers player o ps

                            BE.Pregame role roles confirmed ->
                                pregame player role roles confirmed

                            BE.Rounds rs ->
                                rounds player model.roundsVm rs

                            BE.FiringRound role roles history ->
                                firingRound player role roles history

                            BE.Complete roles history endCondition ->
                                complete roles history endCondition

                            BE.Aborted _ ->
                                H.text "GAME ABORTED"
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
            H.button [ HA.class "btn primary", HE.onClick (Page.ChildMsg StartGame) ] [ H.text "Start Game" ]

          else
            H.text ""
        ]


pregame : Session.Player -> BE.Role -> Dict.Dict BE.PlayerId BE.CensoredRole -> BE.PlayersMap Bool -> H.Html PageMsg
pregame player role roles confirmed =
    H.div [ HA.class "pregame" ]
        [ H.h3 [] [ H.text "Your Role: " ]
        , roleToHtml role
        , H.h3 [] [ H.text "Other roles: " ]
        , H.ul []
            (Dict.remove player.playerId confirmed
                |> Dict.toList
                |> List.map
                    (\( pId, c ) ->
                        H.li []
                            [ H.text pId
                            , H.text " "
                            , H.text (Dict.get pId roles |> Maybe.map censoredRoleToString |> Maybe.withDefault "Unknown Role")
                            , H.text " "
                            , if c then
                                H.text "CONFIRMED"

                              else
                                H.text ""
                            ]
                    )
            )
        , if Dict.get player.playerId confirmed |> Maybe.withDefault False then
            H.text "Confirmed!"

          else
            H.button [ HA.class "btn primary", HE.onClick (Page.ChildMsg ConfirmRole) ] [ H.text "confirm" ]
        ]


rounds : Session.Player -> RoundsViewModel -> BE.ViewRoundsState -> H.Html PageMsg
rounds player vm rs =
    H.div []
        [ H.h2 [] [ H.text ("Round " ++ (List.length rs.roundsHistoric |> String.fromInt)) ]
        , H.p [] [ H.text ("Your role is: " ++ roleToText rs.roundsMyRole) ]
        , H.p [] [ H.text ("The current team lead is: " ++ rs.roundsCurrentLeader) ]
        , case rs.roundsCurrent.currentRoundProposal of
            BE.NoProposal ->
                if player.playerId == rs.roundsCurrentLeader then
                    teamProposalView
                        player.playerId
                        rs.roundsLeadershipQueue
                        vm.teamSelected
                        rs.roundsCurrent.currentRoundShape.roundShapeTeamSize

                else
                    H.text "Waiting for team proposal from the team lead! Now is the time to plead your case in chat! :D"

            BE.Proposed team votes ->
                H.div []
                    ([ H.p [] [ H.text "The proposed team is: " ]
                     , H.ul [] (List.map (\p -> H.li [] [ H.text p ]) team)
                     ]
                        ++ (if List.member player.playerId votes then
                                [ H.text "Voted!" ]

                            else if player.playerId == rs.roundsCurrentLeader then
                                [ H.text "Waiting for votes for the other players" ]

                            else
                                [ H.button [ HE.onClick (Page.ChildMsg (VoteOnTeam False)), HA.class "btn scary" ] [ H.text "Reject" ]
                                , H.text " "
                                , H.button [ HE.onClick (Page.ChildMsg (VoteOnTeam True)), HA.class "btn friendly" ] [ H.text "Approve" ]
                                ]
                           )
                    )

            BE.Approved team votes ->
                H.div []
                    ([ H.p [] [ H.text "Project Team:" ]
                     , H.ul [] (List.map (\p -> H.li [] [ H.text p ]) team)
                     ]
                        ++ (if List.member player.playerId team then
                                if List.member player.playerId votes then
                                    [ H.text "Voted!" ]

                                else
                                    (if not (isCrusader rs.roundsMyRole) then
                                        [ H.button [ HE.onClick (Page.ChildMsg (VoteOnProject False)), HA.class "btn scary" ] [ H.text "Sabotage Project" ]
                                        , H.text " "
                                        ]

                                     else
                                        []
                                    )
                                        ++ [ H.button [ HE.onClick (Page.ChildMsg (VoteOnProject True)), HA.class "btn friendly" ] [ H.text "Pass Project" ] ]

                            else
                                [ H.text "Waiting for project vote by team members..." ]
                           )
                    )
        ]


isCrusader : BE.Role -> Bool
isCrusader r =
    case r of
        BE.CompositionalCrusaders _ ->
            True

        _ ->
            False


firingRound : Session.Player -> BE.Role -> BE.PlayersMap BE.CensoredRole -> List BE.HistoricRoundState -> H.Html PageMsg
firingRound player role roles history =
    H.div []
        [ H.h2 [] [ H.text "Firing Round" ]
        , H.p [] [ H.text "The boss now has a chance to fire the FP expert and claim victory for the side effect team." ]
        , case role of
            BE.SneakySideEffects True ->
                firingView player roles

            _ ->
                H.text "Waiting for the boss"
        ]


complete : BE.PlayersMap BE.Role -> BE.EndCondition -> List BE.HistoricRoundState -> H.Html PageMsg
complete roles endCondition history =
    H.div []
        [ H.h2 [] [ H.text "Game Ended" ]
        , H.p [] [ H.text (endConditionText endCondition) ]
        , H.h3 [] [ H.text "Player Roles" ]
        , H.ul []
            (Dict.toList roles |> List.map (\( pId, r ) -> H.li [] [ H.text pId, H.text ": ", H.text (roleToText r) ]))
        ]


firingView : Session.Player -> BE.PlayersMap BE.CensoredRole -> H.Html PageMsg
firingView player roles =
    H.ul []
        (Dict.remove player.playerId roles
            |> Dict.keys
            |> List.map
                (\p ->
                    H.li []
                        [ H.text p
                        , H.text " "
                        , H.button [ HA.class "btn scary", HE.onClick (Page.ChildMsg (FirePlayer p)) ] [ H.text "Fire" ]
                        ]
                )
        )


teamProposalView : BE.PlayerId -> List BE.PlayerId -> Set BE.PlayerId -> Int -> H.Html PageMsg
teamProposalView leaderId players selected teamSize =
    let
        isAtMaximum =
            Set.size selected >= teamSize

        isSelected pId =
            Set.member pId selected

        teamMemberProposal pId =
            H.li []
                [ H.text pId
                , H.text " "
                , H.button
                    [ HE.onClick (Page.ChildMsg (ToggleTeamMemberProposal pId))
                    , HA.class
                        ("btn "
                            ++ (if isSelected pId then
                                    "scary"

                                else
                                    "friendly"
                               )
                        )
                    , HA.disabled (not (isSelected pId) && isAtMaximum)
                    ]
                    [ if isSelected pId then
                        H.text "Remove"

                      else
                        H.text "Propose"
                    ]
                ]
    in
    H.div []
        [ H.h2 [] [ H.text "Propose a team" ]
        , H.ul [] (List.map teamMemberProposal players)
        , if isAtMaximum then
            H.button [ HA.class "btn primary", HE.onClick (Page.ChildMsg ConfirmTeamProposal) ] [ H.text "Confirm Team Proposal" ]

          else
            H.text ("Please select " ++ String.fromInt teamSize ++ " team members.")
        ]


censoredRoleToString : BE.CensoredRole -> String
censoredRoleToString cr =
    case cr of
        BE.Censored ->
            "Unknown"

        BE.CensoredSideEffect ->
            "Sneaky Side Effect"


roleToHtml : BE.Role -> H.Html msg
roleToHtml r =
    H.div []
        [ H.text (roleToText r)
        , case r of
            BE.CompositionalCrusaders False ->
                H.ul []
                    [ H.li [] [ H.text "Wins by having three successful projects and avoiding the FP expert getting fired at the end." ]
                    , H.li [] [ H.text "Your role is to deduce the side effecting sabotuers and only approve teams with functional allies. :)" ]
                    , H.li [] [ H.text "In the project voting ballot for FP vs Imperative, you must vote for FP." ]
                    ]

            BE.CompositionalCrusaders True ->
                H.ul []
                    [ H.li [] [ H.text "Their FP expertise allows them to know who is supportive of FP and those out to sabotage things." ]
                    , H.li [] [ H.text "Wins by having three successful projects and not getting fired by the middle manager at the end." ]
                    , H.li [] [ H.text "Your role is to guide your fellow FPers to picking the right teams without giving yourself away and getting fired." ]
                    , H.li [] [ H.text "In the project voting ballot for success/fail, you must vote succeed." ]
                    ]

            BE.SneakySideEffects False ->
                H.ul []
                    [ H.li [] [ H.text "Actively against FP and wants it gone." ]
                    , H.li [] [ H.text "If on a project team, can decide to vote for an FP success or an imperative victory" ]
                    , H.li [] [ H.text "It only takes one fail vote to fail a project (except the 4th round with 7 or more players, which requires two failures). Side effects need three wins to take the game." ]
                    , H.li [] [ H.text "If five team proposals are rejected in a round, then the side effects also win." ]
                    , H.li [] [ H.text "All side effects know the identities of all of their FP hating compatriots." ]
                    ]

            BE.SneakySideEffects True ->
                H.ul []
                    [ H.li [] [ H.text "Actively against FP and wants it gone. Same mechanics as the side effects" ]
                    , H.li [] [ H.text "If the crusaders get three fp projects over the line, the boss has one final chance to win. If they can guess the FP Expert and fire them with a single chance, then FP is banished forever! :-/" ]
                    ]
        ]


roleToText : BE.Role -> String
roleToText r =
    case r of
        BE.CompositionalCrusaders False ->
            "Compositional Crusader"

        BE.CompositionalCrusaders True ->
            "FP Expert"

        BE.SneakySideEffects False ->
            "Sneaky Side Effect"

        BE.SneakySideEffects True ->
            "FP Hating Boss"


chatWarnings : NEL.Nonempty String -> H.Html PageMsg
chatWarnings errors =
    H.li [ HA.class "chat-warnings" ] [ H.ul [ HA.class "warn" ] (List.map (\em -> H.li [] [ H.text em ]) (NEL.toList errors)) ]


chatLineView : BE.ViewStateEvent -> H.Html PageMsg
chatLineView ge =
    case ge.viewStateEventData of
        BE.ViewStateEventChat cl ->
            H.p []
                [ H.b [] [ H.text cl.chatLinePlayerId, H.text "> " ]
                , H.text cl.chatLineText
                ]

        BE.ViewStateEventOutput oe ->
            H.p [ HA.class "chat-event" ]
                (case oe of
                    BE.PlayerAdded pId ->
                        [ H.text ("Player Joined: " ++ pId) ]

                    BE.PlayerRemoved pId ->
                        [ H.text ("Player Quit: " ++ pId) ]

                    BE.PregameStarted _ ->
                        [ H.text "Pregame Commenced." ]

                    BE.PlayerConfirmed pId ->
                        [ H.text ("Player " ++ pId ++ " Confirmed Roles") ]

                    BE.RoundsCommenced rs ->
                        [ H.text ("Round 1 Started! " ++ rs.roundsCurrentLeader ++ " is the project leader!") ]

                    BE.TeamProposed pIds ->
                        [ H.text ("Team proposed: " ++ String.join "," pIds) ]

                    BE.TeamApproved votes ->
                        [ H.text ("Team approved with " ++ String.fromInt votes ++ " votes!") ]

                    BE.TeamRejected votes newLeader ->
                        [ H.text
                            ("Team rejected with only "
                                ++ String.fromInt votes
                                ++ " votes! Project Leader is now "
                                ++ newLeader
                            )
                        ]

                    BE.NextRound projectSucceeded failVotes ->
                        [ H.text "FP Project "
                        , H.text
                            (if projectSucceeded then
                                "successful"

                             else
                                "failed"
                            )
                        , H.text ("! (" ++ String.fromInt failVotes ++ " fail votes)")
                        ]

                    BE.ThreeSuccessfulProjects ->
                        [ H.text "FP Project successful! Crusaders have three wins. Middle manager has a chance to fire the FP Expert and claim victory!" ]

                    BE.PlayerFired pId ->
                        [ H.text ("Boss fired player " ++ pId) ]

                    BE.GameEnded roles ec ->
                        [ H.text ("Game Ended. " ++ endConditionText ec) ]

                    BE.GameAborted pId ->
                        [ H.text ("Game aborted by " ++ pId) ]

                    BE.GameCrashed ->
                        [ H.text "Game crashed" ]
                )


endConditionText : BE.EndCondition -> String
endConditionText ec =
    case ec of
        BE.CrusadersWin ->
            "Compositional Crusaders Win"

        BE.SideEffectsWin subEc ->
            "Sneaky Side Effects Win: "
                ++ (case subEc of
                        BE.FPExpertFired ->
                            "FP Expert Fired."

                        BE.ThreeFailedProjects ->
                            "Three Failed Projects."

                        BE.FiveTeamsVetoed ->
                            "Five teams rejected in round."
                   )

        BE.GameCancelled ->
            "Game Cancelled"
