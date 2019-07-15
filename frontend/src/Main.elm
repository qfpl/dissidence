module Main exposing (main)

import Browser
import Generated.Api as BE
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode as Json
import Result
import String
import Task
import Time
import Url


main : Program () Model Msg
main =
    Browser.application
        { init = \_ _ _ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequest
        , onUrlChange = SetUrl
        }


type alias Model =
    { loggedInUsername : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.batch [] )


initModel : Model
initModel =
    { loggedInUsername = Nothing
    }


type Msg
    = UrlRequest Browser.UrlRequest
    | SetUrl Url.Url


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        SetUrl _ ->
            ( model, Cmd.none )

        UrlRequest _ ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Dissidence : Compositional Crusaders"
    , body =
        [ H.div [] [] ]
    }


httpErrorToStr : Http.Error -> String
httpErrorToStr err =
    case err of
        Http.BadUrl s ->
            "Bad URL: " ++ s

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "NetworkError"

        Http.BadStatus s ->
            "Bad Status" ++ String.fromInt s

        Http.BadBody s ->
            "Bad Body" ++ s


onEnterPressed : msg -> H.Attribute msg
onEnterPressed msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail ""
    in
    HE.on "keydown" (Json.andThen isEnter HE.keyCode)
