module Session exposing (User, userDecode, userEncode)

import Browser.Navigation
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (custom)
import Json.Encode as Encode


type alias User =
    { username : String
    }


userEncode : User -> Encode.Value
userEncode u =
    Encode.object [ ( "username", Encode.string u.username ) ]


userDecode : Decode.Decoder User
userDecode =
    Decode.map User (Decode.field "username" Decode.string)
