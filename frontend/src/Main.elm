module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode as Json
import Page.Lobby
import Page.Login
import Page.Register
import Ports
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
    = NotFound (Maybe Session.User)
    | Login Page.Login.Model
    | Register Page.Register.Model
    | Lobby Page.Lobby.Model


type ParentMsg = SetSession (Maybe Session.User)

type PageMsg a 
    = ParentMessage ParentMsg -- A message that child pages can send to the parent
    | ChildMsg a -- A message that is just for the child

liftPageMsg : 

type Msg
    = UrlRequest Browser.UrlRequest
    | SetUrl Url.Url
    | LoginMsg Page.Login.Msg
    | RegisterMsg Page.Register.Msg
    | LobbyMsg Page.Lobby.Msg


type alias Model =
    { key : Nav.Key
    , page : Page
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    initPage key Nothing url


initPage : Nav.Key -> Maybe Session.User -> Url.Url -> ( Model, Cmd Msg )
initPage key user url =
    let
        wrapInit wrapModel wrapMsg ( m, c ) =
            ( { key = key, page = wrapModel m }, Cmd.map wrapMsg c )

        routeMay =
            Url.Parser.parse Route.parser url
    in
    case routeMay of
        Nothing ->
            ( { key = key, page = NotFound user }, Route.pushRoute key Route.Login )

        Just Route.Login ->
            wrapInit Login LoginMsg (Page.Login.init key user)

        Just Route.Register ->
            wrapInit Register RegisterMsg (Page.Register.init key user)

        Just Route.Lobby ->
            wrapInit Register RegisterMsg (Page.Register.init key user)

        Just (Route.Game _) ->
            ( { key = key, page = NotFound user }, Cmd.none )



-- This is gross. We should find a way to push the setSession event up rather than peek into sub states


pageSession : Page -> Maybe Session.User
pageSession p =
    case p of
        NotFound s ->
            s

        Login _ ->
            Nothing

        Register _ ->
            Nothing

        Lobby s ->
            Just s.user


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case ( action, model.page ) of
        ( SetUrl u, _ ) ->
            initPage model.key (pageSession model.page) u

        ( LoginMsg subMsg, Login subModel ) ->
            Page.Login.update subMsg subModel |> updateWith Login LoginMsg model

        ( RegisterMsg subMsg, Register subModel ) ->
            Page.Register.update subMsg subModel |> updateWith Register RegisterMsg model

        ( LobbyMsg subMsg, Lobby subModel ) ->
            Page.Lobby.update subMsg subModel |> updateWith Lobby LobbyMsg model

        ( _, _ ) ->
            ( model, Cmd.none )


updateWith : (submodel -> Page) -> (submsg -> Msg) -> Model -> ( submodel, Cmd submsg ) -> ( Model, Cmd Msg )
updateWith wrapModel wrapMsg model ( subModel, subCmd ) =
    ( { model | page = wrapModel subModel }, Cmd.map wrapMsg subCmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        NotFound _ ->
            Sub.none

        Login sm ->
            Sub.map LoginMsg (Page.Login.subscriptions sm)

        Register sm ->
            Sub.map RegisterMsg (Page.Register.subscriptions sm)

        Lobby sm ->
            Sub.map LobbyMsg (Page.Lobby.subscriptions sm)


view : Model -> Browser.Document Msg
view model =
    case model.page of
        NotFound _ ->
            { title = "Dissidence: Compositional Crusaders", body = [] }

        Login sm ->
            mapDocument LoginMsg (Page.Login.view sm)

        Register sm ->
            mapDocument RegisterMsg (Page.Register.view sm)

        Lobby sm ->
            mapDocument LobbyMsg (Page.Lobby.view sm)


mapDocument : (subMsg -> Msg) -> Browser.Document subMsg -> Browser.Document Msg
mapDocument f { title, body } =
    { title = title, body = List.map (H.map f) body }
