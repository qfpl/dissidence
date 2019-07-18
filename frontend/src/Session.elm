module Session exposing (User, userEncode)

import Browser.Navigation
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (custom)
import Json.Encode as Encode


type alias User =
    { username : String
    }


userEncode : User -> Encode.Value
userEncode _ =
    Encode.null
