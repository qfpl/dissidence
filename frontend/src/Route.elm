module Route exposing (Route(..), parser, pushRoute)

import Browser.Navigation as Nav
import Url.Builder exposing (relative)
import Url.Parser exposing ((</>), Parser, int, map, oneOf, s, string, top)


type Route
    = Login
    | Register
    | Lobby
    | Game Int


parser : Parser (Route -> a) a
parser =
    oneOf
        [ map Lobby top
        , map Register (s "register")
        , map Login (s "login")
        , map Game (s "game" </> int)
        ]


toString : Route -> String
toString r =
    case r of
        Login ->
            relative [ "login" ] []

        Register ->
            relative [ "register" ] []

        Lobby ->
            relative [ "lobby" ] []

        Game gId ->
            relative [ "game", String.fromInt gId ] []


pushRoute : Nav.Key -> Route -> Cmd a
pushRoute k =
    toString >> Nav.pushUrl k
