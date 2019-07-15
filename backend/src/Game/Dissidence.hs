{-# LANGUAGE ConstraintKinds, DataKinds, DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell, TypeApplications, TypeOperators                                  #-}
module Game.Dissidence where

import Control.Lens

--import           Control.Monad.Error.Lens           (throwing)
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
import Game.Dissidence.Db     (AsDbError (..), AsDbLogicError (..), AsSQLiteResponse (..), ChatLine,
                               DbConstraints, DbError (..), DbGameState, DbUser, GameId (..), NewChatLine,
                               Posix, checkLogin, initDb, insertChatLine, insertUser, selectChatLines,
                               selectGameState)
import Game.Dissidence.Env    (Env, configToEnv, withEnvDbConnection)

data AppError = AppServantErr ServantErr | AppDbError DbError deriving (Show, Generic)
makeClassyPrisms ''AppError

instance AsDbError AppError where
  _DbError = _AppDbError . _DbError

instance AsDbLogicError AppError where
  _DbLogicError = _AppDbError . _DbLogicError

instance AsSQLiteResponse AppError where
  _SQLiteResponse = _AppDbError . _SQLiteResponse

type AppConstraints e r m = (DbConstraints e r m, AsAppError e)

type ChatApi
  =    QueryParam "since" Posix :> Get '[JSON] [ChatLine]
  :<|> ReqBody '[JSON] NewChatLine :> Post '[JSON] ()

type GameApi = Get '[JSON] DbGameState

type UserApi
  =    ReqBody '[JSON] DbUser :> Post '[JSON] ()

type LoginApi
  =    ReqBody '[JSON] DbUser :> Post '[JSON] Bool

type Api = "api" :>
  ("lobby" :> ChatApi
  :<|> "game" :> GameApi
  :<|> "user" :> UserApi
  :<|> "login" :> LoginApi
  )

api :: Proxy Api
api = Proxy

server :: AppConstraints e r m => ServerT Api m
server = (globalChatGet :<|> globalChatAppend) :<|> gameGet :<|> userPost :<|> loginPost

globalChatGet :: AppConstraints e r m => Maybe Posix -> m [ChatLine]
globalChatGet _ = selectChatLines Nothing

globalChatAppend :: AppConstraints e r m => NewChatLine -> m ()
globalChatAppend = insertChatLine

gameGet :: AppConstraints e r m => m DbGameState
gameGet = selectGameState (GameId 1)

userPost :: AppConstraints e r m => DbUser -> m ()
userPost = insertUser

-- TODO: Actually do proper auth stuff
loginPost :: AppConstraints e r m => DbUser -> m Bool
loginPost = checkLogin

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
