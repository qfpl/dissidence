module Session exposing (Session, User, userEncode)

import Browser.Navigation
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (custom)
import Json.Encode as Encode


type alias Session =
    { key : Browser.Navigation.Key
    , user : Maybe User
    }


type alias User =
    { username : String
    }


userEncode : User -> Encode.Value
userEncode _ =
    Encode.null
