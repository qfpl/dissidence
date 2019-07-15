module Utils exposing (httpErrorToStr, maybeToList, onEnterPressed)

import Html as H
import Http


maybe : b -> (a -> b) -> Maybe a -> b
maybe z f =
    Maybe.map f >> Maybe.withDefault z


maybeToList : Maybe a -> List a
maybeToList =
    maybe [] List.singleton


httpErrorToStr : Http.Error -> String
httpErrorToStr err =
    case err of
        Http.BadUrl s ->
            "Bad URL: " ++ s

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "NetworkError"

        Http.BadStatus s ->
            "Bad Status" ++ String.fromInt s

        Http.BadBody s ->
            "Bad Body" ++ s


onEnterPressed : msg -> H.Attribute msg
onEnterPressed msg =
    let
        isEnter code =
            if code == 13 then
                Json.succeed msg

            else
                Json.fail ""
    in
    HE.on "keydown" (Json.andThen isEnter HE.keyCode)
