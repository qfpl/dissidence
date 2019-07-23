{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, LambdaCase, MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators #-}
{-# LANGUAGE UndecidableInstances                                                                  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           Control.Lens
import qualified Data.Text                    as T
import           Elm.TyRep
import           GHC.TypeLits                 (ErrorMessage (Text), KnownSymbol, Symbol, TypeError, symbolVal)
import           Servant.Auth
import           Servant.Auth.Client          (Token)
import           Servant.Elm
import           Servant.Elm.Internal.Foreign
import           Servant.Foreign              hiding (Static)

import Debug.Trace

import Game.Dissidence
import Game.Dissidence.Db
import Game.Dissidence.GameState

instance IsElmDefinition Token where
  compileElmDef _ = ETypePrimAlias (EPrimAlias (ETypeName "Token" []) (ETyCon (ETCon "String")))

type family TokenHeaderName xs :: Symbol where
  TokenHeaderName (Cookie ': xs) = "X-XSRF-TOKEN"
  TokenHeaderName (JWT ': xs) = "Authorization"
  TokenHeaderName (x ': xs) = TokenHeaderName xs
  TokenHeaderName '[] = TypeError (Text "Neither JWT nor cookie auth enabled")

instance
  ( TokenHeaderName auths ~ header
  , KnownSymbol header
  , HasForeignType lang ftype Token
  , HasForeign lang ftype sub
  , Show ftype
  )
  => HasForeign lang ftype (Auth auths a :> sub) where
    type Foreign ftype (Auth auths a :> sub) = Foreign ftype sub

    foreignFor lang Proxy Proxy req =
      foreignFor lang Proxy subP $ req & reqHeaders <>~ [HeaderArg arg]
      where
        arg   = Arg
          { _argName = PathSegment . T.pack $ symbolVal @header Proxy
          , _argType = token
          }
        token = typeFor lang (Proxy @ftype) (Proxy @Token)
        subP  = Proxy @sub

myElmOpts :: ElmOptions
myElmOpts = defElmOptions
  { urlPrefix = Static "http://localhost:8001"
  , stringElmTypes =
    [ toElmType (Proxy @String)
    , toElmType (Proxy @T.Text)
    , toElmType (Proxy @Token)
    ]
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
  , "jsonDecBool : Json.Decode.Decoder Bool"
  , "jsonDecBool = Json.Decode.bool"
  , "jsonEncBool : Bool -> Value"
  , "jsonEncBool = Json.Encode.bool"
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
    , DefineElm (Proxy :: Proxy Token)
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
    , DefineElm (Proxy :: Proxy DbUser)
    , DefineElm (Proxy :: Proxy DbGameState)
    ]
    (Proxy :: Proxy Api)
