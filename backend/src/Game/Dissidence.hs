{-# LANGUAGE DataKinds, DeriveGeneric, OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}
{-# LANGUAGE TypeApplications, TypeOperators                                                   #-}
module Game.Dissidence where

import Data.Text                (Text)
import GHC.Generics             (Generic)
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Elm              (defaultOptions, deriveBoth)

import Game.Dissidence.Config    (load)
import Game.Dissidence.Env       (Env, configToEnv)
import Game.Dissidence.GameState (GameState, PlayerId (..), newGame)

type Posix = Integer

data ChatLine = ChatLine
  { chatLineTime     :: Posix
  , chatLineUsername :: Text
  , chatLineText     :: Text
  } deriving (Show, Generic)

data NewChatLine = NewChatLine
  { newChatLineUsername :: Text
  , newChatLineText     :: Text
  } deriving (Show, Generic)

type ChatApi =
  QueryParam "since" Posix :> Get '[JSON] [ChatLine]
  :<|> ReqBody '[JSON] NewChatLine :> Post '[JSON] ()

concat <$> mapM
  (deriveBoth defaultOptions)
  [''ChatLine, ''NewChatLine]

type GameApi = Get '[JSON] GameState

type Api = "api" :>
  ("lobby" :> ChatApi
  :<|> "game" :> GameApi
  )

api :: Proxy Api
api = Proxy

server :: ServerT Api Handler
server = (globalChatGet :<|> globalChatAppend) :<|> gameGet

globalChatGet :: Maybe Posix -> Handler [ChatLine]
globalChatGet _ = pure []

globalChatAppend :: NewChatLine -> Handler ()
globalChatAppend _ = pure ()

gameGet :: Handler GameState
gameGet = pure (newGame (PlayerId "P1"))

app :: Env -> Application
app _ = serve api (hoistServer api id server)

runApp :: IO ()
runApp = do
  c  <- load "./config" -- TODO: Opts for path
  e  <- configToEnv c
  run 8001 (app e)
