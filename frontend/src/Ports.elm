port module Ports exposing (onUserSessionChange, putUserSession)

import Json.Decode exposing (Value)
import Session


port putUserSessionValue : Maybe Value -> Cmd msg


port onUserSessionValueChange : (Value -> msg) -> Sub msg


putUserSession : Maybe Session.User -> Cmd a
putUserSession u =
    Maybe.map Session.userEncode u |> putUserSessionValue


onUserSessionChange : (Maybe Session.User -> msg) -> Sub msg
onUserSessionChange f =
    onUserSessionValueChange (Json.Decode.decodeValue Session.userDecode >> Result.toMaybe >> f)
