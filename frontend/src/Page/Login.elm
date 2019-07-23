module Page.Login exposing (Model, Msg, init, subscriptions, update, view)

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
import Route
import Session
import Utils exposing (disabledIfLoading, maybe, maybeToList, remoteDataError)


type Msg
    = SetUsername String
    | SetPassword String
    | Submit
    | HandleResp (Result Http.Error BE.Token)


type alias Model =
    { username : String
    , password : String
    , submission : RemoteData String Session.User
    }


type alias PageMsg =
    Page.SubMsg Msg


init : Nav.Key -> Maybe Session.User -> ( Model, Cmd PageMsg )
init key user =
    ( { username = ""
      , password = ""
      , submission = RemoteData.NotAsked
      }
    , Utils.maybe Cmd.none (always (Route.pushRoute key Route.Lobby)) user
    )


subscriptions : Maybe Session.User -> Model -> Sub PageMsg
subscriptions _ _ =
    Ports.onUserSessionChange (Page.wrapParentMsg (Page.SetUser Route.Lobby))


update : Nav.Key -> Maybe Session.User -> Msg -> Model -> ( Model, Cmd PageMsg )
update key user msg model =
    case msg of
        SetUsername u ->
            ( { model | username = u }, Cmd.none )

        SetPassword p ->
            ( { model | password = p }, Cmd.none )

        Submit ->
            ( { model | submission = RemoteData.Loading }
            , BE.postApiLogin
                { dbUsername = model.username, dbUserPassword = model.password }
                (Page.wrapChildMsg HandleResp)
            )

        HandleResp r ->
            let
                remoteData =
                    RemoteData.fromResult r
                        |> RemoteData.mapError Utils.httpErrorToStr
                        |> RemoteData.map (\t -> { username = model.username, token = t })
            in
            ( { model | submission = remoteData }
            , RemoteData.unwrap
                Cmd.none
                (\us -> Cmd.batch [ putUserSession (Just us) ])
                remoteData
            )


view : Maybe Session.User -> Model -> Browser.Document Msg
view user model =
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
