port module Ports exposing (onUserSessionChange, putUserSession)

import Json.Decode exposing (Value)
import Session


port putUserSessionValue : Maybe Value -> Cmd msg


port onUserSessionValueChange : (Value -> msg) -> Sub msg


putUserSession : Maybe Session.User -> (Maybe Session.User -> msg) -> Cmd msg
putUserSession u f =
    Maybe.map Session.userEncode u |> putUserSessionValue |> Cmd.map (always (f u))


onUserSessionChange : (Maybe Session.User -> msg) -> Sub msg
onUserSessionChange f =
    onUserSessionChange f
