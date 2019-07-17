port module Ports exposing (onUserSessionChange, putUserSession)

import Json.Decode exposing (Value)
import Session


port putUserSessionValue : Maybe Value -> Cmd msg


port onUserSessionValueChange : (Value -> msg) -> Sub msg


putUserSession : Maybe Session.User -> Cmd msg
putUserSession =
    Maybe.map Session.userEncode >> putUserSessionValue


onUserSessionChange : (Maybe Session.User -> msg) -> Sub msg
onUserSessionChange f =
    onUserSessionChange f
