module Main exposing (main)

import Browser
import Browser.Navigation
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode as Json
import Page.Login
import Result
import String
import Task
import Time
import Url


type alias Flags =
    ()


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


type Model
    = Prelogin
    | Login Page.Login.Model


init : Flags -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( m, c ) =
            Page.Login.init
    in
    ( Login m, Cmd.map LoginMsg c )


initModel : Model
initModel =
    Prelogin


type Msg
    = UrlRequest Browser.UrlRequest
    | SetUrl Url.Url
    | LoginMsg Page.Login.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case ( action, model ) of
        ( LoginMsg subMsg, Login subModel ) ->
            Page.Login.update subMsg subModel |> updateWith Login LoginMsg

        ( _, _ ) ->
            ( model, Cmd.none )


updateWith : (submodel -> Model) -> (submsg -> Msg) -> ( submodel, Cmd submsg ) -> ( Model, Cmd Msg )
updateWith wrapModel wrapMsg ( subModel, subCmd ) =
    ( wrapModel subModel, Cmd.map wrapMsg subCmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    case model of
        Prelogin ->
            { title = "Dissidence: Compositional Crusaders", body = [] }

        Login lm ->
            mapDocument LoginMsg (Page.Login.view lm)


mapDocument : (subMsg -> Msg) -> Browser.Document subMsg -> Browser.Document Msg
mapDocument f { title, body } =
    { title = title, body = List.map (H.map f) body }
