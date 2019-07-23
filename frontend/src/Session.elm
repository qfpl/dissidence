module Session exposing (User, userDecode, userEncode)

import Browser.Navigation
import Generated.Api exposing (Token)
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (custom)
import Json.Encode as Encode


type alias User =
    { username : String
    , token : Token
    }


userEncode : User -> Encode.Value
userEncode u =
    Encode.object [ ( "username", Encode.string u.username ), ( "token", Encode.string u.token ) ]


userDecode : Decode.Decoder User
userDecode =
    Decode.map2 User (Decode.field "username" Decode.string) (Decode.field "token" Decode.string)
