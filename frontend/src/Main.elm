module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Debug
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Http
import Json.Decode as Json
import Page
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
import Utils


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


type Page login register lobby
    = Login login
    | Register register
    | Lobby lobby


type alias PageMsg =
    Page Page.Login.Msg Page.Register.Msg Page.Lobby.Msg


type alias PageModel =
    Page Page.Login.Model Page.Register.Model Page.Lobby.Model


type Msg
    = UrlRequest Browser.UrlRequest
    | SetUrl Url.Url
    | HandleParentMsg Page.ParentMsg
    | HandlePageMsg PageMsg


type alias Model =
    { key : Nav.Key -- Key has to be in this spot for elm-hot to be happy. :)
    , user : Maybe Session.User
    , page : Maybe PageModel
    }


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    initPage key Nothing url


initPage : Nav.Key -> Maybe Session.User -> Url.Url -> ( Model, Cmd Msg )
initPage key user url =
    let
        wrapInit : (a -> PageModel) -> (b -> PageMsg) -> ( a, Cmd (Page.SubMsg b) ) -> ( Model, Cmd Msg )
        wrapInit wrapModel wrapMsg ( m, c ) =
            ( { key = key, user = user, page = Just (wrapModel m) }, Cmd.map (liftSubMsg wrapMsg) c )

        routeMay =
            Url.Parser.parse Route.parser url

        redirect r =
            ( { key = key, user = user, page = Nothing }, Route.pushRoute key r )

        requireUser f =
            Utils.maybe (redirect Route.Login) f user
    in
    case Debug.log "INIT PAGE" routeMay of
        Nothing ->
            redirect Route.Login

        Just Route.Login ->
            wrapInit Login Login (Page.Login.init key user)

        Just Route.Register ->
            wrapInit Register Register (Page.Register.init key user)

        Just Route.Lobby ->
            requireUser (\u -> wrapInit Lobby Lobby (Page.Lobby.init key u))

        Just (Route.Game _) ->
            ( { key = key, user = Nothing, page = Nothing }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case Debug.log "UPDATE" action of
        UrlRequest _ ->
            ( model, Cmd.none )

        SetUrl u ->
            initPage model.key model.user u

        HandleParentMsg m ->
            case m of
                Page.SetUser u ->
                    ( { model | user = u }, Cmd.none )

        HandlePageMsg pm ->
            let
                requireUser f =
                    Utils.maybe ( model, Route.pushRoute model.key Route.Login ) f model.user
            in
            case ( pm, model.page ) of
                ( Login subMsg, Just (Login subModel) ) ->
                    Page.Login.update model.key model.user subMsg subModel |> updateWith Login Login model

                ( Register subMsg, Just (Register subModel) ) ->
                    Page.Register.update model.key model.user subMsg subModel |> updateWith Register Register model

                ( Lobby subMsg, Just (Lobby subModel) ) ->
                    requireUser (\u -> Page.Lobby.update model.key u subMsg subModel |> updateWith Lobby Lobby model)

                ( _, _ ) ->
                    ( model, Cmd.none )


updateWith :
    (submodel -> PageModel)
    -> (submsg -> PageMsg)
    -> Model
    -> ( submodel, Cmd (Page.SubMsg submsg) )
    -> ( Model, Cmd Msg )
updateWith wrapModel wrapMsg model ( subModel, subCmd ) =
    ( { model | page = Just (wrapModel subModel) }, Cmd.map (liftSubMsg wrapMsg) subCmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        requireUser f =
            Utils.maybe Sub.none f model.user
    in
    case Debug.log "SUBS" model.page of
        Nothing ->
            Sub.none

        Just (Login sm) ->
            Sub.map (liftSubMsg Login) (Page.Login.subscriptions model.user sm)

        Just (Register sm) ->
            Sub.map (liftSubMsg Register) (Page.Register.subscriptions model.user sm)

        Just (Lobby sm) ->
            requireUser (\u -> Sub.map (liftSubMsg Lobby) (Page.Lobby.subscriptions u sm))


view : Model -> Browser.Document Msg
view model =
    case Debug.log "VIEW" model.page of
        Nothing ->
            { title = "Dissidence: Compositional Crusaders", body = [] }

        Just (Login sm) ->
            mapDocument (wrapPageMsg Login) (Page.Login.view model.user sm)

        Just (Register sm) ->
            mapDocument (wrapPageMsg Register) (Page.Register.view sm)

        Just (Lobby sm) ->
            mapDocument (wrapPageMsg Lobby) (Page.Lobby.view sm)


mapDocument : (subMsg -> Msg) -> Browser.Document subMsg -> Browser.Document Msg
mapDocument f { title, body } =
    { title = title, body = List.map (H.map f) body }


liftSubMsg : (a -> PageMsg) -> Page.SubMsg a -> Msg
liftSubMsg pageMsg pm =
    case pm of
        Page.ParentMsg m ->
            HandleParentMsg m

        Page.ChildMsg c ->
            HandlePageMsg (pageMsg c)


wrapPageMsg : (a -> PageMsg) -> a -> Msg
wrapPageMsg f =
    f >> HandlePageMsg
