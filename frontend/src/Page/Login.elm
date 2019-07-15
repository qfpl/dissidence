module Page.Login exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Html as H
import Html.Attributes as HA
import Html.Events as HE


type Msg
    = SetUsername String
    | SetPassword String
    | Submit


type alias Model =
    { username : String
    , password : String
    , errorMessage : Maybe String
    }


view : Model -> Browser.Document Msg
view lm =
    { title = "Dissidence: Compositional Crusaders - Login"
    , body =
        [ H.h1 [] [ H.text "Login" ]
        , H.form [ HE.onSubmit Submit ]
            [ H.input
                [ HA.placeholder "Username"
                , HE.onInput SetUsername
                ]
                []
            , H.input
                [ HA.placeholder "Password"
                , HA.type_ "password"
                , HE.onInput SetPassword
                ]
                []
            , H.button [] [ H.text "Login" ]
            ]
        ]
    }


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
            ( { model | errorMessage = Just "Not implemented" }, Cmd.none )


init : ( Model, Cmd Msg )
init =
    ( { username = "", password = "", errorMessage = Nothing }, Cmd.none )
