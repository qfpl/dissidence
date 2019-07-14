{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module Main where

import qualified Data.Text   as T
import           Elm.TyRep
import           Servant.Elm

import Debug.Trace

import Game.Dissidence
import Game.Dissidence.Db
import Game.Dissidence.GameState

myElmOpts :: ElmOptions
myElmOpts = defElmOptions
  { urlPrefix = Static ""
  }

myElmImports :: T.Text
myElmImports = T.unlines
  [ "import Json.Decode"
  , "import Json.Encode exposing (Value)"
  , "-- The following module comes from bartavelle/json-helpers"
  , "import Json.Helpers exposing (..)"
  , "import Dict exposing (Dict)"
  , "import Set"
  , "import Http"
  , "import String"
  , "import Url.Builder"
  , "import Time exposing (Posix, posixToMillis, millisToPosix)"
  , ""
  , "maybeBoolToIntStr : Maybe Bool -> String"
  , "maybeBoolToIntStr mx ="
  , "  case mx of"
  , "    Nothing -> \"\""
  , "    Just True -> \"1\""
  , "    Just False -> \"0\""
  , ""
  , "jsonDecPosix : Json.Decode.Decoder Posix"
  , "jsonDecPosix = Json.Decode.map Time.millisToPosix Json.Decode.int"
  , "jsonEncPosix : Posix -> Value"
  , "jsonEncPosix = posixToMillis >> Json.Encode.int"
  ]

main :: IO ()
main =
  generateElmModuleWith
    myElmOpts
    [ "Generated"
    , "Api"
    ]
    myElmImports
    "../frontend/api/"
    [ DefineElm (Proxy :: Proxy GameId)
    , DefineElm (Proxy :: Proxy ChatLine)
    , DefineElm (Proxy :: Proxy NewChatLine)
    , DefineElm (Proxy :: Proxy PlayerId)
    , DefineElm (Proxy :: Proxy CrusaderRole)
    , DefineElm (Proxy :: Proxy SideEffectRole)
    , DefineElm (Proxy :: Proxy Role)
    , DefineElm (Proxy :: Proxy LeadershipQueue)
    , DefineElm (Proxy :: Proxy SideEffectWinCondition)
    , DefineElm (Proxy :: Proxy EndCondition)
    , DefineElm (Proxy :: Proxy RoundShape)
    , DefineElm (Proxy :: Proxy ProposalState)
    , DefineElm (Proxy :: Proxy TeamVotingResult)
    , DefineElm (Proxy :: Proxy RoundResult)
    , DefineElm (Proxy :: Proxy CurrentRoundState)
    , DefineElm (Proxy :: Proxy HistoricRoundState)
    , DefineElm (Proxy :: Proxy RoundsState)
    , DefineElm (Proxy :: Proxy GameState)
    , DefineElm (Proxy :: Proxy DbGameState)
    ]
    (Proxy :: Proxy Api)
