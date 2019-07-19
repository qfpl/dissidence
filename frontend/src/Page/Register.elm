module Page.Register exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Generated.Api as BE
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Http
import Page
import Ports exposing (putUserSession)
import RemoteData exposing (RemoteData)
import Result
import Route
import Session
import Utils exposing (disabledIfLoading, maybe, maybeToList, remoteDataError)


type Msg
    = SetUsername String
    | SetPassword String
    | SetPasswordAgain String
    | Submit
    | HandleResp (Result Http.Error ())


type alias Model =
    { username : String
    , password : String
    , passwordAgain : String
    , validationIssues : List String
    , submission : RemoteData String Session.User
    }


init : Nav.Key -> Maybe Session.User -> ( Model, Cmd PageMsg )
init key user =
    ( { username = ""
      , password = ""
      , passwordAgain = ""
      , validationIssues = []
      , submission = RemoteData.NotAsked
      }
    , maybe Cmd.none (always (Route.pushRoute key Route.Lobby)) user
    )


type alias PageMsg =
    Page.SubMsg Msg


subscriptions : Maybe Session.User -> Model -> Sub PageMsg
subscriptions _ _ =
    Sub.none


update : Nav.Key -> Maybe Session.User -> Msg -> Model -> ( Model, Cmd PageMsg )
update key user msg model =
    case msg of
        SetUsername u ->
            ( { model | username = u }, Cmd.none )

        SetPassword p ->
            ( { model | password = p }, Cmd.none )

        SetPasswordAgain p ->
            ( { model | passwordAgain = p }, Cmd.none )

        Submit ->
            case validateDbUser model of
                Ok dbUser ->
                    ( { model | validationIssues = [], submission = RemoteData.Loading }
                    , BE.postApiUser dbUser (Page.wrapChildMsg HandleResp)
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
                            (always { username = model.username })
            in
            ( { model | submission = remoteData }
            , RemoteData.unwrap
                Cmd.none
                (\us ->
                    Cmd.batch
                        [ putUserSession (Just us), Route.pushRoute key Route.Lobby ]
                )
                remoteData
            )



-- elm-verify is a much better way of doing this. But this is our only validation.
-- Come back to this later.


validateDbUser : Model -> Result.Result (List String) BE.DbUser
validateDbUser model =
    let
        trimmedUser =
            String.trim model.username

        usernameError =
            if trimmedUser == "" then
                [ "Username cannot be blank" ]

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
            List.concat [ usernameError, passwordError, mismatchError ]
    in
    if allErrs == [] then
        Result.Ok { dbUsername = trimmedUser, dbUserPassword = model.password }

    else
        Result.Err allErrs


view : Model -> Browser.Document Msg
view model =
    { title = "Dissidence: Compositional Crusaders - Register"
    , body =
        [ H.div [ HA.class "login-box" ]
            [ H.h1 [] [ H.text "Register" ]
            , H.form [ HE.onSubmit Submit ]
                [ H.input
                    [ HA.placeholder "Username"
                    , HE.onInput SetUsername
                    , HA.value model.username
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
