module Page.Register exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Generated.Api as BE
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Http
import Page
import Ports exposing (putPlayerSession)
import RemoteData exposing (RemoteData)
import Result
import Route
import Session
import Utils exposing (disabledIfLoading, maybe, maybeToList, remoteDataError)


type Msg
    = SetPlayerId String
    | SetPassword String
    | SetPasswordAgain String
    | Submit
    | HandleResp (Result Http.Error String)


type alias Model =
    { playerId : String
    , password : String
    , passwordAgain : String
    , validationIssues : List String
    , submission : RemoteData String Session.Player
    }


init : Nav.Key -> Maybe Session.Player -> ( Model, Cmd PageMsg )
init key player =
    ( { playerId = ""
      , password = ""
      , passwordAgain = ""
      , validationIssues = []
      , submission = RemoteData.NotAsked
      }
    , maybe Cmd.none (always (Route.pushRoute key Route.Lobby)) player
    )


type alias PageMsg =
    Page.SubMsg Msg


subscriptions : Maybe Session.Player -> Model -> Sub PageMsg
subscriptions _ _ =
    Sub.none


update : Nav.Key -> Maybe Session.Player -> Msg -> Model -> ( Model, Cmd PageMsg )
update key player msg model =
    case msg of
        SetPlayerId pId ->
            ( { model | playerId = pId }, Cmd.none )

        SetPassword p ->
            ( { model | password = p }, Cmd.none )

        SetPasswordAgain p ->
            ( { model | passwordAgain = p }, Cmd.none )

        Submit ->
            case validateDbPlayer model of
                Ok dbPlayer ->
                    ( { model | validationIssues = [], submission = RemoteData.Loading }
                    , BE.postApiPlayers dbPlayer (Page.wrapChildMsg HandleResp)
                    )

                Err problems ->
                    ( { model
                        | submission = RemoteData.NotAsked
                        , validationIssues = problems
                      }
                    , Cmd.none
                    )

        HandleResp r ->
            let
                remoteData =
                    RemoteData.fromResult r
                        |> RemoteData.mapError Utils.httpErrorToStr
                        |> RemoteData.map
                            (\t -> { playerId = model.playerId, token = t })
            in
            ( { model | submission = remoteData }
            , RemoteData.unwrap
                Cmd.none
                (\us ->
                    Cmd.batch
                        [ putPlayerSession (Just us), Route.pushRoute key Route.Lobby ]
                )
                remoteData
            )



-- elm-verify is a much better way of doing this. But this is our only validation.
-- Come back to this later.


validateDbPlayer : Model -> Result.Result (List String) BE.DbPlayer
validateDbPlayer model =
    let
        trimmedPlayerId =
            String.trim model.playerId

        playerIdError =
            if trimmedPlayerId == "" then
                [ "PlayerID cannot be blank" ]

            else
                []

        passwordError =
            if model.password == "" then
                [ "Password cannot be blank" ]

            else
                []

        mismatchError =
            if model.password /= model.passwordAgain then
                [ "Passwords do not match" ]

            else
                []

        allErrs =
            List.concat [ playerIdError, passwordError, mismatchError ]
    in
    if allErrs == [] then
        Result.Ok { dbPlayerId = trimmedPlayerId, dbPlayerPassword = model.password }

    else
        Result.Err allErrs


view : Model -> Browser.Document Msg
view model =
    { title = "Dissidence: Compositional Crusaders - Register"
    , body =
        [ H.div [ HA.class "login-box" ]
            [ H.h1 [] [ H.text "Register" ]
            , H.p [ HA.class "login-alternative" ]
                [ H.text "Already have a player id? "
                , Route.routeLink Route.Login [ H.text "Login" ]
                , H.text " instead!"
                ]
            , H.form [ HE.onSubmit Submit ]
                [ H.input
                    [ HA.placeholder "Player ID"
                    , HE.onInput SetPlayerId
                    , HA.value model.playerId
                    ]
                    []
                , H.input
                    [ HA.placeholder "Password"
                    , HA.type_ "password"
                    , HE.onInput SetPassword
                    , HA.value model.password
                    ]
                    []
                , H.input
                    [ HA.placeholder "Confirm Password"
                    , HA.type_ "password"
                    , HE.onInput SetPasswordAgain
                    , HA.value model.passwordAgain
                    ]
                    []
                , H.ul [ HA.class "warn" ]
                    (model.submission
                        |> remoteDataError
                        |> maybeToList
                        |> List.append model.validationIssues
                        |> List.map (\em -> H.li [] [ H.text em ])
                    )
                , H.button
                    [ HA.class "btn primary", disabledIfLoading model.submission ]
                    [ H.text "Login" ]
                ]
            ]
        ]
    }
