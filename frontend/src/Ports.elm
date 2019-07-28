port module Ports exposing (onPlayerSessionChange, putPlayerSession)

import Json.Decode exposing (Value)
import Session


port putPlayerSessionValue : Maybe Value -> Cmd msg


port onPlayerSessionValueChange : (Value -> msg) -> Sub msg


putPlayerSession : Maybe Session.Player -> Cmd a
putPlayerSession p =
    Maybe.map Session.playerEncode p |> putPlayerSessionValue


onPlayerSessionChange : (Maybe Session.Player -> msg) -> Sub msg
onPlayerSessionChange f =
    onPlayerSessionValueChange (Json.Decode.decodeValue Session.playerDecode >> Result.toMaybe >> f)
