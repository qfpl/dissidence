module Generated.Api exposing(..)

-- This is not so generated atm. Gotta fix servant-elm for 0.19 support. RIP.

import Json.Decode
import Json.Encode exposing (Value)
-- The following module comes from bartavelle/json-helpers
import Json.Helpers exposing (..)
import Dict exposing (Dict)
import Set
import Http
import List
import String
import Iso8601
import Url.Builder
import Time exposing (Posix)

maybeBoolToIntStr : Maybe Bool -> String
maybeBoolToIntStr mx =
  case mx of
    Nothing -> ""
    Just True -> "1"
    Just False -> "0"

type alias ChatLine  =
   { chatLineTime: Posix
   , chatLineUsername: String
   , chatLineText: String
   }

jsonDecChatLine : Json.Decode.Decoder ( ChatLine )
jsonDecChatLine =
   Json.Decode.succeed (\pchatLineTime pchatLineUsername pchatLineText -> {chatLineTime = pchatLineTime, chatLineUsername = pchatLineUsername, chatLineText = pchatLineText})
   |> required "chatLineTime" (Iso8601.decoder)
   |> required "chatLineUsername" (Json.Decode.string)
   |> required "chatLineText" (Json.Decode.string)

jsonEncChatLine : ChatLine -> Value
jsonEncChatLine  val =
   Json.Encode.object
   [ ("chatLineTime", Iso8601.encode val.chatLineTime)
   , ("chatLineUsername", Json.Encode.string val.chatLineUsername)
   , ("chatLineText", Json.Encode.string val.chatLineText)
   ]



type alias NewChatLine  =
   { newChatLineUsername: String
   , newChatLineText: String
   }

jsonDecNewChatLine : Json.Decode.Decoder ( NewChatLine )
jsonDecNewChatLine =
   Json.Decode.succeed (\pnewChatLineUsername pnewChatLineText -> {newChatLineUsername = pnewChatLineUsername, newChatLineText = pnewChatLineText})
   |> required "newChatLineUsername" (Json.Decode.string)
   |> required "newChatLineText" (Json.Decode.string)

jsonEncNewChatLine : NewChatLine -> Value
jsonEncNewChatLine  val =
   Json.Encode.object
   [ ("newChatLineUsername", Json.Encode.string val.newChatLineUsername)
   , ("newChatLineText", Json.Encode.string val.newChatLineText)
   ]


getLobby : (Maybe Posix) -> (Result Http.Error  ((List ChatLine))  -> msg) -> Cmd msg
getLobby query_since toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [ [ query_since
                    |> Maybe.map (Iso8601.fromTime >> Url.Builder.string "since") ]
                ])
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                Url.Builder.relative
                    [ "api"
                    , "lobby"
                    ]
                    params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson toMsg (Json.Decode.list jsonDecChatLine)
            , timeout =
                Nothing
            , tracker =
                Nothing
            }

postLobby : NewChatLine -> (Result Http.Error ()  -> msg) -> Cmd msg
postLobby body toMsg =
    let
        params =
            List.filterMap identity
            (List.concat
                [])
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                Url.Builder.relative
                    [ "api"
                    , "lobby"
                    ]
                    params
            , body =
                Http.jsonBody (jsonEncNewChatLine body)
            , expect =
                Http.expectWhatever toMsg
            , timeout =
                Nothing
            , tracker =
                Nothing
            }
