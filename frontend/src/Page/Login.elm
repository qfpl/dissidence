module Page.Login exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Generated.Api as BE
import Html as H
import Html.Attributes as HA
import Html.Attributes.Aria as HAA
import Html.Events as HE
import Http
import Page
import Ports exposing (putPlayerSession)
import RemoteData exposing (RemoteData)
import Route
import Session
import Utils exposing (disabledIfLoading, maybe, maybeToList, remoteDataError)


type Msg
    = SetPlayerId String
    | SetPassword String
    | Submit
    | HandleResp (Result Http.Error BE.Token)


type alias Model =
    { playerId : String
    , password : String
    , submission : RemoteData String Session.Player
    }


type alias PageMsg =
    Page.SubMsg Msg


init : Nav.Key -> Maybe Session.Player -> ( Model, Cmd PageMsg )
init key player =
    ( { playerId = ""
      , password = ""
      , submission = RemoteData.NotAsked
      }
    , Utils.maybe Cmd.none (always (Route.pushRoute key Route.Lobby)) player
    )


subscriptions : Maybe Session.Player -> Model -> Sub PageMsg
subscriptions _ _ =
    Ports.onPlayerSessionChange (Page.wrapParentMsg (Page.SetPlayer Route.Lobby))


update : Nav.Key -> Maybe Session.Player -> Msg -> Model -> ( Model, Cmd PageMsg )
update key player msg model =
    case msg of
        SetPlayerId pId ->
            ( { model | playerId = pId }, Cmd.none )

        SetPassword p ->
            ( { model | password = p }, Cmd.none )

        Submit ->
            ( { model | submission = RemoteData.Loading }
            , BE.postApiLogin
                { dbPlayerId = model.playerId, dbPlayerPassword = model.password }
                (Page.wrapChildMsg HandleResp)
            )

        HandleResp r ->
            let
                remoteData =
                    RemoteData.fromResult r
                        |> RemoteData.mapError Utils.httpErrorToStr
                        |> RemoteData.map (\t -> { playerId = model.playerId, token = t })
            in
            ( { model | submission = remoteData }
            , RemoteData.unwrap
                Cmd.none
                (\us -> Cmd.batch [ putPlayerSession (Just us) ])
                remoteData
            )


view : Maybe Session.Player -> Model -> Browser.Document Msg
view player model =
    { title = "Dissidence - Login"
    , body =
        [ H.div [ HA.class "login-box" ]
            [ H.h1 [] [ H.text "Login" ]
            , H.p [ HA.class "login-alternative" ]
                [ H.text "Don't have a player id? "
                , Route.routeLink Route.Register [ H.text "Register" ]
                , H.text " instead!"
                ]
            , H.form [ HE.onSubmit Submit ]
                [ H.input
                    [ HA.placeholder "Player Id"
                    , HE.onInput SetPlayerId
                    , HA.value model.playerId
                    , HAA.ariaLabel "Player ID"
                    ]
                    []
                , H.input
                    [ HA.placeholder "Password"
                    , HA.type_ "password"
                    , HE.onInput SetPassword
                    , HA.value model.password
                    , HAA.ariaLabel "Password"
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
