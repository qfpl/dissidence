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
import Page.Game
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


main : Program (Maybe Session.Player) Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequest
        , onUrlChange = SetUrl
        }


type Page login register lobby game
    = Login login
    | Register register
    | Lobby lobby
    | Game game


type alias PageMsg =
    Page Page.Login.Msg Page.Register.Msg Page.Lobby.Msg Page.Game.Msg


type alias PageModel =
    Page Page.Login.Model Page.Register.Model Page.Lobby.Model Page.Game.Model


type Msg
    = UrlRequest Browser.UrlRequest
    | SetUrl Url.Url
    | HandleParentMsg Page.ParentMsg
    | HandlePageMsg PageMsg


type alias Model =
    { key : Nav.Key -- Key has to be in this spot for elm-hot to be happy. :)
    , player : Maybe Session.Player
    , page : Maybe PageModel
    }


init : Maybe Session.Player -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init player url key =
    initPage key player url


initPage : Nav.Key -> Maybe Session.Player -> Url.Url -> ( Model, Cmd Msg )
initPage key player url =
    let
        wrapInit : (a -> PageModel) -> (b -> PageMsg) -> ( a, Cmd (Page.SubMsg b) ) -> ( Model, Cmd Msg )
        wrapInit wrapModel wrapMsg ( m, c ) =
            ( { key = key, player = player, page = Just (wrapModel m) }, Cmd.map (liftSubMsg wrapMsg) c )

        routeMay =
            Url.Parser.parse Route.parser url

        redirect r =
            ( { key = key, player = player, page = Nothing }, Route.pushRoute key r )

        requirePlayer f =
            Utils.maybe (redirect Route.Login) f player
    in
    case routeMay of
        Nothing ->
            redirect Route.Login

        Just Route.Login ->
            wrapInit Login Login (Page.Login.init key player)

        Just Route.Register ->
            wrapInit Register Register (Page.Register.init key player)

        Just Route.Lobby ->
            requirePlayer (\p -> wrapInit Lobby Lobby (Page.Lobby.init key p))

        Just (Route.Game gId) ->
            requirePlayer (\p -> wrapInit Game Game (Page.Game.init key p gId))


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        UrlRequest urlReq ->
            case urlReq of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
                    )

        SetUrl u ->
            initPage model.key model.player u

        HandleParentMsg m ->
            case m of
                Page.SetPlayer r p ->
                    ( { model | player = p }, Route.pushRoute model.key r )

                Page.Logout ->
                    ( { model | player = Nothing }
                    , Cmd.batch [ Ports.putPlayerSession Nothing, Route.pushRoute model.key Route.Login ]
                    )

        HandlePageMsg pm ->
            let
                requirePlayer f =
                    Utils.maybe ( model, Route.pushRoute model.key Route.Login ) f model.player
            in
            case ( pm, model.page ) of
                ( Login subMsg, Just (Login subModel) ) ->
                    Page.Login.update model.key model.player subMsg subModel |> updateWith Login Login model

                ( Register subMsg, Just (Register subModel) ) ->
                    Page.Register.update model.key model.player subMsg subModel |> updateWith Register Register model

                ( Lobby subMsg, Just (Lobby subModel) ) ->
                    requirePlayer (\p -> Page.Lobby.update model.key p subMsg subModel |> updateWith Lobby Lobby model)

                ( Game subMsg, Just (Game subModel) ) ->
                    requirePlayer (\p -> Page.Game.update model.key p subMsg subModel |> updateWith Game Game model)

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
        requirePlayer f =
            Utils.maybe Sub.none f model.player
    in
    case model.page of
        Nothing ->
            Sub.none

        Just (Login sm) ->
            Sub.map (liftSubMsg Login) (Page.Login.subscriptions model.player sm)

        Just (Register sm) ->
            Sub.map (liftSubMsg Register) (Page.Register.subscriptions model.player sm)

        Just (Lobby sm) ->
            requirePlayer (\p -> Sub.map (liftSubMsg Lobby) (Page.Lobby.subscriptions p sm))

        Just (Game sm) ->
            requirePlayer (\p -> Sub.map (liftSubMsg Game) (Page.Game.subscriptions p sm))


view : Model -> Browser.Document Msg
view model =
    let
        blankDoc =
            { title = "Dissidence: Compositional Crusaders", body = [] }

        requirePlayer f =
            Utils.maybe blankDoc f model.player
    in
    case model.page of
        Nothing ->
            blankDoc

        Just (Login sm) ->
            mapDocument (wrapPageMsg Login) (Page.Login.view model.player sm)

        Just (Register sm) ->
            mapDocument (wrapPageMsg Register) (Page.Register.view sm)

        Just (Lobby sm) ->
            requirePlayer (\p -> mapDocument (liftSubMsg Lobby) (Page.Lobby.view p sm))

        Just (Game sm) ->
            requirePlayer (\p -> mapDocument (liftSubMsg Game) (Page.Game.view p sm))


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
