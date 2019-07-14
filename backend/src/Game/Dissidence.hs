{-# LANGUAGE ConstraintKinds, DataKinds, DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell, TypeApplications, TypeOperators                                  #-}
module Game.Dissidence where

import Control.Lens

import           Control.Monad.Error.Lens           (throwing)
import           Control.Monad.Except               (ExceptT, runExceptT)
import           Control.Monad.IO.Class             (MonadIO, liftIO)
import           Control.Monad.Reader               (ReaderT, runReaderT)
import qualified Data.ByteString.Lazy.Char8         as C8
import           Data.Generics.Product              (field)
import           Database.SQLite.Simple             (Connection)
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)
import           GHC.Generics                       (Generic)
import           Network.Wai.Handler.Warp           (run)
import           Servant

import Game.Dissidence.Config (load)
import Game.Dissidence.Db     (AsSQLiteResponse (..), ChatLine, DbConstraints, DbGameState, GameId (..),
                               NewChatLine, Posix, initDb, insertChatLine, selectChatLines, selectGameState)
import Game.Dissidence.Env    (Env, configToEnv, withEnvDbConnection)

type ChatApi =
  QueryParam "since" Posix :> Get '[JSON] [ChatLine]
  :<|> ReqBody '[JSON] NewChatLine :> Post '[JSON] ()

type GameApi = Get '[JSON] DbGameState

data AppError = AppServantErr ServantErr | AppDbError SQLiteResponse deriving (Show, Generic)
makeClassyPrisms ''AppError

instance AsSQLiteResponse AppError where
  _SQLiteResponse = _AppDbError . _SQLiteResponse

type AppConstraints e r m = (DbConstraints e r m, AsAppError e)

type Api = "api" :>
  ("lobby" :> ChatApi
  :<|> "game" :> GameApi
  )

api :: Proxy Api
api = Proxy

server :: AppConstraints e r m => ServerT Api m
server = (globalChatGet :<|> globalChatAppend) :<|> gameGet

globalChatGet :: AppConstraints e r m => Maybe Posix -> m [ChatLine]
globalChatGet _ = selectChatLines Nothing

globalChatAppend :: AppConstraints e r m => NewChatLine -> m ()
globalChatAppend = insertChatLine

gameGet :: AppConstraints e r m => m DbGameState
gameGet = do
  gMay <- selectGameState (GameId 1)
  maybe (throwing _AppServantErr err404) pure gMay

app :: Env -> Application
app e = serve api (hoistServer api (appHandler e) server)

runDbContext :: (AsSQLiteResponse e, MonadIO m) => Env -> ExceptT e (ReaderT Connection IO) a -> m (Either e a)
runDbContext e = liftIO . withEnvDbConnection e . runReaderT . runExceptT

appHandler :: Env -> ExceptT AppError (ReaderT Connection IO) a -> Handler a
appHandler e prog = do
  res <- runDbContext e prog
  case res of
    Left (AppDbError err)    -> throwError $ err500 { errBody = "Database error: " <> C8.pack (show err) }
    Left (AppServantErr err) -> throwError err
    Right a                  -> pure a

runApp :: IO ()
runApp = do
  c  <- load "./config" -- TODO: Opts for path
  e  <- configToEnv c
  putStrLn $ "Opening/Initialising sqlite database " <> (e ^. field @"dbPath")
  initRes <- runDbContext e initDb
  case initRes of
    Left (err ::SQLiteResponse) -> error $ "DB Failed to initialise: " <> (show err)
    Right _  -> do
      let port = c ^. field @"port" . to fromIntegral
      putStrLn $ "Starting server on port " <> (show port)
      run port (app e)
