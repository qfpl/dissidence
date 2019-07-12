{-# LANGUAGE DataKinds, DeriveGeneric, OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}
{-# LANGUAGE TypeApplications, TypeOperators                                                   #-}
module Game.Dissidence where

import Control.Lens

import           Control.Monad.Except               (ExceptT, runExceptT)
import           Control.Monad.IO.Class             (MonadIO, liftIO)
import           Control.Monad.Reader               (ReaderT, runReaderT)
import qualified Data.ByteString.Lazy.Char8         as C8
import           Data.Generics.Product              (field)
import           Data.Text                          (Text)
import           Database.SQLite.Simple             (Connection)
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)
import           GHC.Generics                       (Generic)
import           Network.Wai.Handler.Warp           (run)
import           Servant
import           Servant.Elm                        (defaultOptions, deriveBoth)

import Game.Dissidence.Config    (load)
import Game.Dissidence.Db        (DbConstraints, initDb)
import Game.Dissidence.Env       (Env, configToEnv, withEnvDbConnection)
import Game.Dissidence.GameState (GameState, PlayerId (..), newGame)


type ChatApi =
  QueryParam "since" Posix :> Get '[JSON] [ChatLine]
  :<|> ReqBody '[JSON] NewChatLine :> Post '[JSON] ()

type GameApi = Get '[JSON] GameState

type Api = "api" :>
  ("lobby" :> ChatApi
  :<|> "game" :> GameApi
  )

api :: Proxy Api
api = Proxy

server :: DbConstraints e r m => ServerT Api m
server = (globalChatGet :<|> globalChatAppend) :<|> gameGet

globalChatGet :: DbConstraints e r m => Maybe Posix -> m [ChatLine]
globalChatGet _ = pure []

globalChatAppend :: DbConstraints e r m => NewChatLine -> m ()
globalChatAppend _ = pure ()

gameGet :: DbConstraints e r m => m GameState
gameGet = pure (newGame (PlayerId "P1"))

app :: Env -> Application
app e = serve api (hoistServer api (appHandler e) server)

runContext :: MonadIO m => Env -> ExceptT SQLiteResponse (ReaderT Connection IO) a -> m (Either SQLiteResponse a)
runContext e = liftIO . withEnvDbConnection e . runReaderT . runExceptT

appHandler :: Env -> ExceptT SQLiteResponse (ReaderT Connection IO) a -> Handler a
appHandler e prog = do
  res <- runContext e prog
  case res of
    Left err -> throwError $ err500 { errBody = "Database error: " <> C8.pack (show err) }
    Right a  -> pure a

runApp :: IO ()
runApp = do
  c  <- load "./config" -- TODO: Opts for path
  e  <- configToEnv c
  putStrLn $ "Opening/Initialising sqlite database " <> (e ^. field @"dbPath")
  initRes <- runContext e initDb
  case initRes of
    Left err -> error $ "DB Failed to initialise: " <> (show err)
    Right _  -> do
      let port = c ^. field @"port" . to fromIntegral
      putStrLn $ "Starting server on port " <> (show port)
      run port (app e)
