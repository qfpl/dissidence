port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Json.Decode exposing (Value)
import Json.Encode
import Url


port putUserSessionValue : Maybe Value -> Cmd msg


port onUserSessionValueChange : (Value -> msg) -> Sub msg


type alias Flags =
    ()


type Msg
    = UrlRequest Browser.UrlRequest
    | SetUrl Url.Url
    | SetSession Value
    | PushSession


type alias Model =
    { value : Maybe Value, clicked : Bool, key : Nav.Key }


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequest
        , onUrlChange = SetUrl
        }


init flags url key =
    ( { value = Nothing, clicked = False, key = key }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetUrl u ->
            ( model, Cmd.none )

        UrlRequest u ->
            ( model, Cmd.none )

        SetSession s ->
            ( { model | value = Just s }, Cmd.none )

        PushSession ->
            ( { model | clicked = True }, putUserSessionValue (Just (Json.Encode.string "Yaaaas")) )


view : Model -> Browser.Document Msg
view model =
    { title = "butts"
    , body =
        [ H.text
            (if model.clicked then
                "butts"

             else
                "assbutts"
            )
        , H.button [ HE.onClick PushSession ] [ H.text "Click Me" ]
        , H.pre [] [ H.text (Maybe.map (Json.Encode.encode 2) model.value |> Maybe.withDefault "") ]
        ]
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    onUserSessionValueChange SetSession
