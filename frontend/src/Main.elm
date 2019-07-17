module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode as Json
import Page.Login
import Page.Register
import Result
import Route
import Session
import String
import Task
import Time
import Url
import Url.Parser


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


type Page
    = Login Page.Login.Model
    | Register Page.Register.Model


type alias Model =
    { key : Nav.Key
    , user : Maybe Session.User
    , page : Maybe Page
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    initPage key Nothing url


initPage : Nav.Key -> Maybe Session.User -> Url.Url -> ( Model, Cmd Msg )
initPage key user url =
    let
        wrapInit wrapModel wrapMsg ( m, c ) =
            ( { key = key, user = user, page = Just (wrapModel m) }, Cmd.map wrapMsg c )

        routeMay =
            Url.Parser.parse Route.parser url
    in
    case routeMay of
        Nothing ->
            ( { key = key, user = user, page = Nothing }, Route.pushRoute key Route.Login )

        Just Route.Login ->
            wrapInit Login LoginMsg (Page.Login.init key user)

        Just Route.Register ->
            wrapInit Register RegisterMsg (Page.Register.init key user)

        _ ->
            ( { key = key, user = user, page = Nothing }, Cmd.none )


type Msg
    = UrlRequest Browser.UrlRequest
    | SetUrl Url.Url
    | LoginMsg Page.Login.Msg
    | RegisterMsg Page.Register.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case ( action, model.page ) of
        ( SetUrl u, _ ) ->
            initPage model.key model.user u

        ( LoginMsg subMsg, Just (Login subModel) ) ->
            Page.Login.update subMsg subModel |> updateWith Login LoginMsg model

        ( RegisterMsg subMsg, Just (Register subModel) ) ->
            Page.Register.update subMsg subModel |> updateWith Register RegisterMsg model

        ( _, _ ) ->
            ( model, Cmd.none )


updateWith : (submodel -> Page) -> (submsg -> Msg) -> Model -> ( submodel, Cmd submsg ) -> ( Model, Cmd Msg )
updateWith wrapModel wrapMsg model ( subModel, subCmd ) =
    ( { model | page = Just (wrapModel subModel) }, Cmd.map wrapMsg subCmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Nothing ->
            Sub.none

        Just (Login sm) ->
            Sub.map LoginMsg (Page.Login.subscriptions sm)

        Just (Register sm) ->
            Sub.map RegisterMsg (Page.Register.subscriptions sm)


view : Model -> Browser.Document Msg
view model =
    case model.page of
        Nothing ->
            { title = "Dissidence: Compositional Crusaders", body = [] }

        Just (Login sm) ->
            mapDocument LoginMsg (Page.Login.view sm)

        Just (Register sm) ->
            mapDocument RegisterMsg (Page.Register.view sm)


mapDocument : (subMsg -> Msg) -> Browser.Document subMsg -> Browser.Document Msg
mapDocument f { title, body } =
    { title = title, body = List.map (H.map f) body }
