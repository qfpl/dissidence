module Page.Login exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Generated.Api as BE
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Http
import Ports exposing (putUserSession)
import RemoteData exposing (RemoteData)
import Route
import Session
import Utils exposing (disabledIfLoading, maybe, maybeToList, remoteDataError)


type Msg
    = SetUsername String
    | SetPassword String
    | Submit
    | HandleResp (Result Http.Error Bool)


type alias Model =
    { key : Nav.Key
    , username : String
    , password : String
    , submission : RemoteData String Session.User
    }


init : Nav.Key -> Maybe Session.User -> ( Model, Cmd Msg )
init key user =
    ( { key = key
      , username = ""
      , password = ""
      , submission = RemoteData.NotAsked
      }
    , maybe Cmd.none (always (Route.pushRoute key Route.Lobby)) user
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetUsername u ->
            ( { model | username = u }, Cmd.none )

        SetPassword p ->
            ( { model | password = p }, Cmd.none )

        Submit ->
            ( { model | submission = RemoteData.Loading }
            , BE.postApiLogin
                { dbUsername = model.username, dbUserPassword = model.password }
                HandleResp
            )

        HandleResp r ->
            let
                remoteData =
                    RemoteData.fromResult r
                        |> RemoteData.mapError Utils.httpErrorToStr
                        |> RemoteData.andThen
                            (\b ->
                                if not b then
                                    RemoteData.Failure "Login Failed"

                                else
                                    RemoteData.succeed { username = model.username }
                            )
            in
            ( { model | submission = remoteData }
            , RemoteData.unwrap
                Cmd.none
                (\us -> Cmd.batch [ putUserSession (Just us), Route.pushRoute model.key Route.Lobby ])
                remoteData
            )


view : Model -> Browser.Document Msg
view model =
    { title = "Dissidence: Compositional Crusaders - Login"
    , body =
        [ H.div [ HA.class "login-box" ]
            [ H.h1 [] [ H.text "Login" ]
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
                , H.ul [ HA.class "warn" ]
                    (Maybe.map (\em -> H.li [] [ H.text em ]) (remoteDataError model.submission)
                        |> maybeToList
                    )
                , H.button
                    [ HA.class "btn primary", disabledIfLoading model.submission ]
                    [ H.text "Login" ]
                ]
            ]
        ]
    }
