module Generated.Api exposing (ChatLine, CrusaderRole(..), CurrentRoundState, DbGameState, DbUser, EndCondition(..), GameId, GameState(..), HistoricRoundState, LeadershipQueue, NewChatLine, PlayerId, ProposalState(..), Role(..), RoundResult(..), RoundShape, RoundsState, SideEffectRole(..), SideEffectWinCondition(..), TeamVotingResult, Token, getApiGame, getApiLobby, jsonDecBool, jsonDecChatLine, jsonDecCrusaderRole, jsonDecCurrentRoundState, jsonDecDbGameState, jsonDecDbUser, jsonDecEndCondition, jsonDecGameId, jsonDecGameState, jsonDecHistoricRoundState, jsonDecLeadershipQueue, jsonDecNewChatLine, jsonDecPlayerId, jsonDecPosix, jsonDecProposalState, jsonDecRole, jsonDecRoundResult, jsonDecRoundShape, jsonDecRoundsState, jsonDecSideEffectRole, jsonDecSideEffectWinCondition, jsonDecTeamVotingResult, jsonDecToken, jsonEncBool, jsonEncChatLine, jsonEncCrusaderRole, jsonEncCurrentRoundState, jsonEncDbGameState, jsonEncDbUser, jsonEncEndCondition, jsonEncGameId, jsonEncGameState, jsonEncHistoricRoundState, jsonEncLeadershipQueue, jsonEncNewChatLine, jsonEncPlayerId, jsonEncPosix, jsonEncProposalState, jsonEncRole, jsonEncRoundResult, jsonEncRoundShape, jsonEncRoundsState, jsonEncSideEffectRole, jsonEncSideEffectWinCondition, jsonEncTeamVotingResult, jsonEncToken, maybeBoolToIntStr, postApiLobby, postApiLogin, postApiUser)

-- The following module comes from bartavelle/json-helpers

import Dict exposing (Dict)
import Http
import Json.Decode
import Json.Encode exposing (Value)
import Json.Helpers exposing (..)
import Set
import String
import Time exposing (Posix, millisToPosix, posixToMillis)
import Url.Builder


maybeBoolToIntStr : Maybe Bool -> String
maybeBoolToIntStr mx =
    case mx of
        Nothing ->
            ""

        Just True ->
            "1"

        Just False ->
            "0"


jsonDecBool : Json.Decode.Decoder Bool
jsonDecBool =
    Json.Decode.bool


jsonEncBool : Bool -> Value
jsonEncBool =
    Json.Encode.bool


jsonDecPosix : Json.Decode.Decoder Posix
jsonDecPosix =
    Json.Decode.map Time.millisToPosix Json.Decode.int


jsonEncPosix : Posix -> Value
jsonEncPosix =
    posixToMillis >> Json.Encode.int


type alias GameId =
    Int


jsonDecGameId : Json.Decode.Decoder GameId
jsonDecGameId =
    Json.Decode.int


jsonEncGameId : GameId -> Value
jsonEncGameId val =
    Json.Encode.int val


type alias Token =
    String


jsonDecToken : Json.Decode.Decoder Token
jsonDecToken =
    Json.Decode.string


jsonEncToken : Token -> Value
jsonEncToken val =
    Json.Encode.string val


type alias ChatLine =
    { chatLineTime : Posix
    , chatLineGameId : Maybe GameId
    , chatLineUsername : String
    , chatLineText : String
    }


jsonDecChatLine : Json.Decode.Decoder ChatLine
jsonDecChatLine =
    Json.Decode.succeed (\pchatLineTime pchatLineGameId pchatLineUsername pchatLineText -> { chatLineTime = pchatLineTime, chatLineGameId = pchatLineGameId, chatLineUsername = pchatLineUsername, chatLineText = pchatLineText })
        |> required "chatLineTime" jsonDecPosix
        |> fnullable "chatLineGameId" jsonDecGameId
        |> required "chatLineUsername" Json.Decode.string
        |> required "chatLineText" Json.Decode.string


jsonEncChatLine : ChatLine -> Value
jsonEncChatLine val =
    Json.Encode.object
        [ ( "chatLineTime", jsonEncPosix val.chatLineTime )
        , ( "chatLineGameId", maybeEncode jsonEncGameId val.chatLineGameId )
        , ( "chatLineUsername", Json.Encode.string val.chatLineUsername )
        , ( "chatLineText", Json.Encode.string val.chatLineText )
        ]


type alias NewChatLine =
    { newChatGameId : Maybe GameId
    , newChatLineUsername : String
    , newChatLineText : String
    }


jsonDecNewChatLine : Json.Decode.Decoder NewChatLine
jsonDecNewChatLine =
    Json.Decode.succeed (\pnewChatGameId pnewChatLineUsername pnewChatLineText -> { newChatGameId = pnewChatGameId, newChatLineUsername = pnewChatLineUsername, newChatLineText = pnewChatLineText })
        |> fnullable "newChatGameId" jsonDecGameId
        |> required "newChatLineUsername" Json.Decode.string
        |> required "newChatLineText" Json.Decode.string


jsonEncNewChatLine : NewChatLine -> Value
jsonEncNewChatLine val =
    Json.Encode.object
        [ ( "newChatGameId", maybeEncode jsonEncGameId val.newChatGameId )
        , ( "newChatLineUsername", Json.Encode.string val.newChatLineUsername )
        , ( "newChatLineText", Json.Encode.string val.newChatLineText )
        ]


type alias PlayerId =
    String


jsonDecPlayerId : Json.Decode.Decoder PlayerId
jsonDecPlayerId =
    Json.Decode.string


jsonEncPlayerId : PlayerId -> Value
jsonEncPlayerId val =
    Json.Encode.string val


type CrusaderRole
    = FPExpert


jsonDecCrusaderRole : Json.Decode.Decoder CrusaderRole
jsonDecCrusaderRole =
    let
        jsonDecDictCrusaderRole =
            Dict.fromList [ ( "FPExpert", FPExpert ) ]
    in
    decodeSumUnaries "CrusaderRole" jsonDecDictCrusaderRole


jsonEncCrusaderRole : CrusaderRole -> Value
jsonEncCrusaderRole val =
    case val of
        FPExpert ->
            Json.Encode.string "FPExpert"


type SideEffectRole
    = MiddleManager


jsonDecSideEffectRole : Json.Decode.Decoder SideEffectRole
jsonDecSideEffectRole =
    let
        jsonDecDictSideEffectRole =
            Dict.fromList [ ( "MiddleManager", MiddleManager ) ]
    in
    decodeSumUnaries "SideEffectRole" jsonDecDictSideEffectRole


jsonEncSideEffectRole : SideEffectRole -> Value
jsonEncSideEffectRole val =
    case val of
        MiddleManager ->
            Json.Encode.string "MiddleManager"


type Role
    = CompositionalCrusaders (Maybe CrusaderRole)
    | SneakySideEffects (Maybe SideEffectRole)


jsonDecRole : Json.Decode.Decoder Role
jsonDecRole =
    let
        jsonDecDictRole =
            Dict.fromList
                [ ( "CompositionalCrusaders", Json.Decode.lazy (\_ -> Json.Decode.map CompositionalCrusaders (Json.Decode.maybe jsonDecCrusaderRole)) )
                , ( "SneakySideEffects", Json.Decode.lazy (\_ -> Json.Decode.map SneakySideEffects (Json.Decode.maybe jsonDecSideEffectRole)) )
                ]

        jsonDecObjectSetRole =
            Set.fromList []
    in
    decodeSumTaggedObject "Role" "tag" "contents" jsonDecDictRole jsonDecObjectSetRole


jsonEncRole : Role -> Value
jsonEncRole val =
    let
        keyval v =
            case v of
                CompositionalCrusaders v1 ->
                    ( "CompositionalCrusaders", encodeValue (maybeEncode jsonEncCrusaderRole v1) )

                SneakySideEffects v1 ->
                    ( "SneakySideEffects", encodeValue (maybeEncode jsonEncSideEffectRole v1) )
    in
    encodeSumTaggedObject "tag" "contents" keyval val


type alias LeadershipQueue =
    List String


jsonDecLeadershipQueue : Json.Decode.Decoder LeadershipQueue
jsonDecLeadershipQueue =
    Json.Decode.list Json.Decode.string


jsonEncLeadershipQueue : LeadershipQueue -> Value
jsonEncLeadershipQueue val =
    Json.Encode.list Json.Encode.string val


type SideEffectWinCondition
    = FPExpertFired
    | ThreeFailedProjects
    | FiveTeamsVetoed


jsonDecSideEffectWinCondition : Json.Decode.Decoder SideEffectWinCondition
jsonDecSideEffectWinCondition =
    let
        jsonDecDictSideEffectWinCondition =
            Dict.fromList [ ( "FPExpertFired", FPExpertFired ), ( "ThreeFailedProjects", ThreeFailedProjects ), ( "FiveTeamsVetoed", FiveTeamsVetoed ) ]
    in
    decodeSumUnaries "SideEffectWinCondition" jsonDecDictSideEffectWinCondition


jsonEncSideEffectWinCondition : SideEffectWinCondition -> Value
jsonEncSideEffectWinCondition val =
    case val of
        FPExpertFired ->
            Json.Encode.string "FPExpertFired"

        ThreeFailedProjects ->
            Json.Encode.string "ThreeFailedProjects"

        FiveTeamsVetoed ->
            Json.Encode.string "FiveTeamsVetoed"


type EndCondition
    = CrusadersWin
    | SideEffectsWin SideEffectWinCondition
    | GameCancelled


jsonDecEndCondition : Json.Decode.Decoder EndCondition
jsonDecEndCondition =
    let
        jsonDecDictEndCondition =
            Dict.fromList
                [ ( "CrusadersWin", Json.Decode.lazy (\_ -> Json.Decode.succeed CrusadersWin) )
                , ( "SideEffectsWin", Json.Decode.lazy (\_ -> Json.Decode.map SideEffectsWin jsonDecSideEffectWinCondition) )
                , ( "GameCancelled", Json.Decode.lazy (\_ -> Json.Decode.succeed GameCancelled) )
                ]

        jsonDecObjectSetEndCondition =
            Set.fromList []
    in
    decodeSumTaggedObject "EndCondition" "tag" "contents" jsonDecDictEndCondition jsonDecObjectSetEndCondition


jsonEncEndCondition : EndCondition -> Value
jsonEncEndCondition val =
    let
        keyval v =
            case v of
                CrusadersWin ->
                    ( "CrusadersWin", encodeValue (Json.Encode.list identity []) )

                SideEffectsWin v1 ->
                    ( "SideEffectsWin", encodeValue (jsonEncSideEffectWinCondition v1) )

                GameCancelled ->
                    ( "GameCancelled", encodeValue (Json.Encode.list identity []) )
    in
    encodeSumTaggedObject "tag" "contents" keyval val


type alias RoundShape =
    { roundShapeTeamSize : Int
    , roundShapeTwoFails : Bool
    }


jsonDecRoundShape : Json.Decode.Decoder RoundShape
jsonDecRoundShape =
    Json.Decode.succeed (\proundShapeTeamSize proundShapeTwoFails -> { roundShapeTeamSize = proundShapeTeamSize, roundShapeTwoFails = proundShapeTwoFails })
        |> required "roundShapeTeamSize" Json.Decode.int
        |> required "roundShapeTwoFails" Json.Decode.bool


jsonEncRoundShape : RoundShape -> Value
jsonEncRoundShape val =
    Json.Encode.object
        [ ( "roundShapeTeamSize", Json.Encode.int val.roundShapeTeamSize )
        , ( "roundShapeTwoFails", Json.Encode.bool val.roundShapeTwoFails )
        ]


type ProposalState
    = NoProposal
    | Proposed (List PlayerId) (List ( PlayerId, Bool ))
    | Approved (List PlayerId) (List ( PlayerId, Bool ))


jsonDecProposalState : Json.Decode.Decoder ProposalState
jsonDecProposalState =
    let
        jsonDecDictProposalState =
            Dict.fromList
                [ ( "NoProposal", Json.Decode.lazy (\_ -> Json.Decode.succeed NoProposal) )
                , ( "Proposed", Json.Decode.lazy (\_ -> Json.Decode.map2 Proposed (Json.Decode.index 0 (Json.Decode.list jsonDecPlayerId)) (Json.Decode.index 1 (Json.Decode.list (Json.Decode.map2 tuple2 (Json.Decode.index 0 jsonDecPlayerId) (Json.Decode.index 1 Json.Decode.bool))))) )
                , ( "Approved", Json.Decode.lazy (\_ -> Json.Decode.map2 Approved (Json.Decode.index 0 (Json.Decode.list jsonDecPlayerId)) (Json.Decode.index 1 (Json.Decode.list (Json.Decode.map2 tuple2 (Json.Decode.index 0 jsonDecPlayerId) (Json.Decode.index 1 Json.Decode.bool))))) )
                ]

        jsonDecObjectSetProposalState =
            Set.fromList []
    in
    decodeSumTaggedObject "ProposalState" "tag" "contents" jsonDecDictProposalState jsonDecObjectSetProposalState


jsonEncProposalState : ProposalState -> Value
jsonEncProposalState val =
    let
        keyval v =
            case v of
                NoProposal ->
                    ( "NoProposal", encodeValue (Json.Encode.list identity []) )

                Proposed v1 v2 ->
                    ( "Proposed", encodeValue (Json.Encode.list identity [ Json.Encode.list jsonEncPlayerId v1, Json.Encode.list (\( t1, t2 ) -> Json.Encode.list identity [ jsonEncPlayerId t1, Json.Encode.bool t2 ]) v2 ]) )

                Approved v1 v2 ->
                    ( "Approved", encodeValue (Json.Encode.list identity [ Json.Encode.list jsonEncPlayerId v1, Json.Encode.list (\( t1, t2 ) -> Json.Encode.list identity [ jsonEncPlayerId t1, Json.Encode.bool t2 ]) v2 ]) )
    in
    encodeSumTaggedObject "tag" "contents" keyval val


type alias TeamVotingResult =
    { votingTeamLeader : PlayerId
    , votingResultTeam : List PlayerId
    , votingResult : List ( PlayerId, Bool )
    }


jsonDecTeamVotingResult : Json.Decode.Decoder TeamVotingResult
jsonDecTeamVotingResult =
    Json.Decode.succeed (\pvotingTeamLeader pvotingResultTeam pvotingResult -> { votingTeamLeader = pvotingTeamLeader, votingResultTeam = pvotingResultTeam, votingResult = pvotingResult })
        |> required "votingTeamLeader" jsonDecPlayerId
        |> required "votingResultTeam" (Json.Decode.list jsonDecPlayerId)
        |> required "votingResult" (Json.Decode.list (Json.Decode.map2 tuple2 (Json.Decode.index 0 jsonDecPlayerId) (Json.Decode.index 1 Json.Decode.bool)))


jsonEncTeamVotingResult : TeamVotingResult -> Value
jsonEncTeamVotingResult val =
    Json.Encode.object
        [ ( "votingTeamLeader", jsonEncPlayerId val.votingTeamLeader )
        , ( "votingResultTeam", Json.Encode.list jsonEncPlayerId val.votingResultTeam )
        , ( "votingResult", Json.Encode.list (\( t1, t2 ) -> Json.Encode.list identity [ jsonEncPlayerId t1, Json.Encode.bool t2 ]) val.votingResult )
        ]


type RoundResult
    = RoundSuccess Int
    | RoundFailure Int
    | RoundNoConsensus


jsonDecRoundResult : Json.Decode.Decoder RoundResult
jsonDecRoundResult =
    let
        jsonDecDictRoundResult =
            Dict.fromList
                [ ( "RoundSuccess", Json.Decode.lazy (\_ -> Json.Decode.map RoundSuccess Json.Decode.int) )
                , ( "RoundFailure", Json.Decode.lazy (\_ -> Json.Decode.map RoundFailure Json.Decode.int) )
                , ( "RoundNoConsensus", Json.Decode.lazy (\_ -> Json.Decode.succeed RoundNoConsensus) )
                ]

        jsonDecObjectSetRoundResult =
            Set.fromList []
    in
    decodeSumTaggedObject "RoundResult" "tag" "contents" jsonDecDictRoundResult jsonDecObjectSetRoundResult


jsonEncRoundResult : RoundResult -> Value
jsonEncRoundResult val =
    let
        keyval v =
            case v of
                RoundSuccess v1 ->
                    ( "RoundSuccess", encodeValue (Json.Encode.int v1) )

                RoundFailure v1 ->
                    ( "RoundFailure", encodeValue (Json.Encode.int v1) )

                RoundNoConsensus ->
                    ( "RoundNoConsensus", encodeValue (Json.Encode.list identity []) )
    in
    encodeSumTaggedObject "tag" "contents" keyval val


type alias CurrentRoundState =
    { currentRoundShape : RoundShape
    , currentRoundProposal : ProposalState
    , currentRoundVotes : List TeamVotingResult
    }


jsonDecCurrentRoundState : Json.Decode.Decoder CurrentRoundState
jsonDecCurrentRoundState =
    Json.Decode.succeed (\pcurrentRoundShape pcurrentRoundProposal pcurrentRoundVotes -> { currentRoundShape = pcurrentRoundShape, currentRoundProposal = pcurrentRoundProposal, currentRoundVotes = pcurrentRoundVotes })
        |> required "currentRoundShape" jsonDecRoundShape
        |> required "currentRoundProposal" jsonDecProposalState
        |> required "currentRoundVotes" (Json.Decode.list jsonDecTeamVotingResult)


jsonEncCurrentRoundState : CurrentRoundState -> Value
jsonEncCurrentRoundState val =
    Json.Encode.object
        [ ( "currentRoundShape", jsonEncRoundShape val.currentRoundShape )
        , ( "currentRoundProposal", jsonEncProposalState val.currentRoundProposal )
        , ( "currentRoundVotes", Json.Encode.list jsonEncTeamVotingResult val.currentRoundVotes )
        ]


type alias HistoricRoundState =
    { historicRoundShape : RoundShape
    , historicRoundTeam : Maybe (List PlayerId)
    , historicRoundVotes : List TeamVotingResult
    , historicRoundResult : RoundResult
    }


jsonDecHistoricRoundState : Json.Decode.Decoder HistoricRoundState
jsonDecHistoricRoundState =
    Json.Decode.succeed (\phistoricRoundShape phistoricRoundTeam phistoricRoundVotes phistoricRoundResult -> { historicRoundShape = phistoricRoundShape, historicRoundTeam = phistoricRoundTeam, historicRoundVotes = phistoricRoundVotes, historicRoundResult = phistoricRoundResult })
        |> required "historicRoundShape" jsonDecRoundShape
        |> fnullable "historicRoundTeam" (Json.Decode.list jsonDecPlayerId)
        |> required "historicRoundVotes" (Json.Decode.list jsonDecTeamVotingResult)
        |> required "historicRoundResult" jsonDecRoundResult


jsonEncHistoricRoundState : HistoricRoundState -> Value
jsonEncHistoricRoundState val =
    Json.Encode.object
        [ ( "historicRoundShape", jsonEncRoundShape val.historicRoundShape )
        , ( "historicRoundTeam", maybeEncode (Json.Encode.list jsonEncPlayerId) val.historicRoundTeam )
        , ( "historicRoundVotes", Json.Encode.list jsonEncTeamVotingResult val.historicRoundVotes )
        , ( "historicRoundResult", jsonEncRoundResult val.historicRoundResult )
        ]


type alias RoundsState =
    { roundsRoles : List ( PlayerId, Role )
    , roundsLeadershipQueue : LeadershipQueue
    , roundsCurrent : CurrentRoundState
    , roundsFuture : List RoundShape
    , roundsHistoric : List HistoricRoundState
    }


jsonDecRoundsState : Json.Decode.Decoder RoundsState
jsonDecRoundsState =
    Json.Decode.succeed (\proundsRoles proundsLeadershipQueue proundsCurrent proundsFuture proundsHistoric -> { roundsRoles = proundsRoles, roundsLeadershipQueue = proundsLeadershipQueue, roundsCurrent = proundsCurrent, roundsFuture = proundsFuture, roundsHistoric = proundsHistoric })
        |> required "roundsRoles" (Json.Decode.list (Json.Decode.map2 tuple2 (Json.Decode.index 0 jsonDecPlayerId) (Json.Decode.index 1 jsonDecRole)))
        |> required "roundsLeadershipQueue" jsonDecLeadershipQueue
        |> required "roundsCurrent" jsonDecCurrentRoundState
        |> required "roundsFuture" (Json.Decode.list jsonDecRoundShape)
        |> required "roundsHistoric" (Json.Decode.list jsonDecHistoricRoundState)


jsonEncRoundsState : RoundsState -> Value
jsonEncRoundsState val =
    Json.Encode.object
        [ ( "roundsRoles", Json.Encode.list (\( t1, t2 ) -> Json.Encode.list identity [ jsonEncPlayerId t1, jsonEncRole t2 ]) val.roundsRoles )
        , ( "roundsLeadershipQueue", jsonEncLeadershipQueue val.roundsLeadershipQueue )
        , ( "roundsCurrent", jsonEncCurrentRoundState val.roundsCurrent )
        , ( "roundsFuture", Json.Encode.list jsonEncRoundShape val.roundsFuture )
        , ( "roundsHistoric", Json.Encode.list jsonEncHistoricRoundState val.roundsHistoric )
        ]


type GameState
    = WaitingForPlayers PlayerId (List PlayerId)
    | Pregame (List ( PlayerId, Role )) (List ( PlayerId, Bool ))
    | Rounds RoundsState
    | FiringRound (List ( PlayerId, Role )) (List HistoricRoundState)
    | Complete (List ( PlayerId, Role )) EndCondition (List HistoricRoundState)
    | Aborted PlayerId


jsonDecGameState : Json.Decode.Decoder GameState
jsonDecGameState =
    let
        jsonDecDictGameState =
            Dict.fromList
                [ ( "WaitingForPlayers", Json.Decode.lazy (\_ -> Json.Decode.map2 WaitingForPlayers (Json.Decode.index 0 jsonDecPlayerId) (Json.Decode.index 1 (Json.Decode.list jsonDecPlayerId))) )
                , ( "Pregame", Json.Decode.lazy (\_ -> Json.Decode.map2 Pregame (Json.Decode.index 0 (Json.Decode.list (Json.Decode.map2 tuple2 (Json.Decode.index 0 jsonDecPlayerId) (Json.Decode.index 1 jsonDecRole)))) (Json.Decode.index 1 (Json.Decode.list (Json.Decode.map2 tuple2 (Json.Decode.index 0 jsonDecPlayerId) (Json.Decode.index 1 Json.Decode.bool))))) )
                , ( "Rounds", Json.Decode.lazy (\_ -> Json.Decode.map Rounds jsonDecRoundsState) )
                , ( "FiringRound", Json.Decode.lazy (\_ -> Json.Decode.map2 FiringRound (Json.Decode.index 0 (Json.Decode.list (Json.Decode.map2 tuple2 (Json.Decode.index 0 jsonDecPlayerId) (Json.Decode.index 1 jsonDecRole)))) (Json.Decode.index 1 (Json.Decode.list jsonDecHistoricRoundState))) )
                , ( "Complete", Json.Decode.lazy (\_ -> Json.Decode.map3 Complete (Json.Decode.index 0 (Json.Decode.list (Json.Decode.map2 tuple2 (Json.Decode.index 0 jsonDecPlayerId) (Json.Decode.index 1 jsonDecRole)))) (Json.Decode.index 1 jsonDecEndCondition) (Json.Decode.index 2 (Json.Decode.list jsonDecHistoricRoundState))) )
                , ( "Aborted", Json.Decode.lazy (\_ -> Json.Decode.map Aborted jsonDecPlayerId) )
                ]

        jsonDecObjectSetGameState =
            Set.fromList []
    in
    decodeSumTaggedObject "GameState" "tag" "contents" jsonDecDictGameState jsonDecObjectSetGameState


jsonEncGameState : GameState -> Value
jsonEncGameState val =
    let
        keyval v =
            case v of
                WaitingForPlayers v1 v2 ->
                    ( "WaitingForPlayers", encodeValue (Json.Encode.list identity [ jsonEncPlayerId v1, Json.Encode.list jsonEncPlayerId v2 ]) )

                Pregame v1 v2 ->
                    ( "Pregame", encodeValue (Json.Encode.list identity [ Json.Encode.list (\( t1, t2 ) -> Json.Encode.list identity [ jsonEncPlayerId t1, jsonEncRole t2 ]) v1, Json.Encode.list (\( t1, t2 ) -> Json.Encode.list identity [ jsonEncPlayerId t1, Json.Encode.bool t2 ]) v2 ]) )

                Rounds v1 ->
                    ( "Rounds", encodeValue (jsonEncRoundsState v1) )

                FiringRound v1 v2 ->
                    ( "FiringRound", encodeValue (Json.Encode.list identity [ Json.Encode.list (\( t1, t2 ) -> Json.Encode.list identity [ jsonEncPlayerId t1, jsonEncRole t2 ]) v1, Json.Encode.list jsonEncHistoricRoundState v2 ]) )

                Complete v1 v2 v3 ->
                    ( "Complete", encodeValue (Json.Encode.list identity [ Json.Encode.list (\( t1, t2 ) -> Json.Encode.list identity [ jsonEncPlayerId t1, jsonEncRole t2 ]) v1, jsonEncEndCondition v2, Json.Encode.list jsonEncHistoricRoundState v3 ]) )

                Aborted v1 ->
                    ( "Aborted", encodeValue (jsonEncPlayerId v1) )
    in
    encodeSumTaggedObject "tag" "contents" keyval val


type alias DbUser =
    { dbUsername : String
    , dbUserPassword : String
    }


jsonDecDbUser : Json.Decode.Decoder DbUser
jsonDecDbUser =
    Json.Decode.succeed (\pdbUsername pdbUserPassword -> { dbUsername = pdbUsername, dbUserPassword = pdbUserPassword })
        |> required "dbUsername" Json.Decode.string
        |> required "dbUserPassword" Json.Decode.string


jsonEncDbUser : DbUser -> Value
jsonEncDbUser val =
    Json.Encode.object
        [ ( "dbUsername", Json.Encode.string val.dbUsername )
        , ( "dbUserPassword", Json.Encode.string val.dbUserPassword )
        ]


type alias DbGameState =
    { dbGameStateId : GameId
    , dbGameState : GameState
    }


jsonDecDbGameState : Json.Decode.Decoder DbGameState
jsonDecDbGameState =
    Json.Decode.succeed (\pdbGameStateId pdbGameState -> { dbGameStateId = pdbGameStateId, dbGameState = pdbGameState })
        |> required "dbGameStateId" jsonDecGameId
        |> required "dbGameState" jsonDecGameState


jsonEncDbGameState : DbGameState -> Value
jsonEncDbGameState val =
    Json.Encode.object
        [ ( "dbGameStateId", jsonEncGameId val.dbGameStateId )
        , ( "dbGameState", jsonEncGameState val.dbGameState )
        ]


getApiLobby : Token -> Maybe Int -> (Result Http.Error (List ChatLine) -> msg) -> Cmd msg
getApiLobby header_Authorization query_since toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    [ [ query_since
                            |> Maybe.map (String.fromInt >> Url.Builder.string "since")
                      ]
                    ]
                )
    in
    Http.request
        { method =
            "GET"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization" << String.fromInt) (Just header_Authorization)
                ]
        , url =
            Url.Builder.crossOrigin "http://localhost:8001"
                [ "api"
                , "lobby"
                ]
                params
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson toMsg (Json.Decode.list jsonDecChatLine)
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


postApiLobby : Token -> NewChatLine -> (Result Http.Error () -> msg) -> Cmd msg
postApiLobby header_Authorization body toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    []
                )
    in
    Http.request
        { method =
            "POST"
        , headers =
            List.filterMap identity
                [ Maybe.map (Http.header "Authorization" << String.fromInt) (Just header_Authorization)
                ]
        , url =
            Url.Builder.crossOrigin "http://localhost:8001"
                [ "api"
                , "lobby"
                ]
                params
        , body =
            Http.jsonBody (jsonEncNewChatLine body)
        , expect =
            Http.expectString
                (\x ->
                    case x of
                        Err e ->
                            toMsg (Err e)

                        Ok _ ->
                            toMsg (Ok ())
                )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


getApiGame : (Result Http.Error DbGameState -> msg) -> Cmd msg
getApiGame toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    []
                )
    in
    Http.request
        { method =
            "GET"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin "http://localhost:8001"
                [ "api"
                , "game"
                ]
                params
        , body =
            Http.emptyBody
        , expect =
            Http.expectJson toMsg jsonDecDbGameState
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


postApiUser : DbUser -> (Result Http.Error () -> msg) -> Cmd msg
postApiUser body toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    []
                )
    in
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin "http://localhost:8001"
                [ "api"
                , "user"
                ]
                params
        , body =
            Http.jsonBody (jsonEncDbUser body)
        , expect =
            Http.expectString
                (\x ->
                    case x of
                        Err e ->
                            toMsg (Err e)

                        Ok _ ->
                            toMsg (Ok ())
                )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }


postApiLogin : DbUser -> (Result Http.Error () -> msg) -> Cmd msg
postApiLogin body toMsg =
    let
        params =
            List.filterMap identity
                (List.concat
                    []
                )
    in
    Http.request
        { method =
            "POST"
        , headers =
            []
        , url =
            Url.Builder.crossOrigin "http://localhost:8001"
                [ "api"
                , "login"
                ]
                params
        , body =
            Http.jsonBody (jsonEncDbUser body)
        , expect =
            Http.expectString
                (\x ->
                    case x of
                        Err e ->
                            toMsg (Err e)

                        Ok _ ->
                            toMsg (Ok ())
                )
        , timeout =
            Nothing
        , tracker =
            Nothing
        }
