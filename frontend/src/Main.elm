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
    { chatLines : List BE.ChatLine
    , chatErr : Maybe Http.Error
    , newChatText : String
    , gameState : Maybe BE.GameState
    , gameStateErr : Maybe Http.Error
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.batch [ BE.getApiLobby Nothing SetChatLines, BE.getApiGame SetGame ] )


initModel : Model
initModel =
    { chatLines = []
    , chatErr = Nothing
    , newChatText = ""
    , gameState = Nothing
    , gameStateErr = Nothing
    }


type Msg
    = FetchChat
    | SetGame (Result Http.Error BE.GameState)
    | SetChatLines (Result Http.Error (List BE.ChatLine))
    | SetNewChatText String
    | SubmitNewChatLine
    | SubmitNewChatLineRes (Result Http.Error ())
    | UrlRequest Browser.UrlRequest
    | SetUrl Url.Url
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        FetchChat ->
            ( model, BE.getApiLobby Nothing SetChatLines )

        SetGame res ->
            ( { model
                | gameState =
                    case res of
                        Ok gs ->
                            Just gs

                        Err _ ->
                            model.gameState
                , gameStateErr =
                    case res of
                        Ok _ ->
                            Nothing

                        Err e ->
                            Just e
              }
            , Cmd.none
            )

        SetChatLines res ->
            ( { model
                | chatLines =
                    case res of
                        Ok ls ->
                            ls

                        Err _ ->
                            model.chatLines
                , chatErr =
                    case res of
                        Ok _ ->
                            Nothing

                        Err e ->
                            Just e
              }
            , Cmd.none
            )

        SetNewChatText t ->
            ( { model | newChatText = t }, Cmd.none )

        SubmitNewChatLine ->
            ( model
            , BE.postApiLobby
                { newChatLineUsername = "user", newChatLineText = model.newChatText }
                SubmitNewChatLineRes
            )

        SubmitNewChatLineRes _ ->
            ( { model | newChatText = "" }, BE.getApiLobby Nothing SetChatLines )

        SetUrl _ ->
            ( model, Cmd.none )

        UrlRequest _ ->
            ( model, Cmd.none )

        Tick _ ->
            ( model, BE.getApiLobby Nothing SetChatLines )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick


view : Model -> Browser.Document Msg
view model =
    { title = "Dissidence : Compositional Crusaders"
    , body =
        [ H.div
            []
            [ H.h1 [] [ H.text "GameState" ]
            , H.text (Maybe.withDefault "" (Maybe.map httpErrorToStr model.gameStateErr))
            ]
        , H.div
            []
            ([ H.h1 [] [ H.text "Chat" ] ]
                ++ List.map
                    (\cl ->
                        H.p []
                            [ H.strong [] [ H.text cl.chatLineUsername ]
                            , H.text ": "
                            , H.text cl.chatLineText
                            ]
                    )
                    model.chatLines
            )
        , H.p []
            [ H.text (Maybe.withDefault "" (Maybe.map httpErrorToStr model.chatErr))
            ]
        , H.input
            [ HA.placeholder "Enter Chat message"
            , HA.value model.newChatText
            , HE.onInput SetNewChatText
            , onEnterPressed SubmitNewChatLine
            ]
            []
        ]
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
