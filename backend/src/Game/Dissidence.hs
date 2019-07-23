{-# LANGUAGE ConstraintKinds, DataKinds, DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell, TypeApplications, TypeOperators                                  #-}
module Game.Dissidence where

import Control.Lens

import           Control.Monad                        (unless)
import           Control.Monad.Error.Lens             (throwing)
import           Control.Monad.Except                 (ExceptT, runExceptT)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.Reader                 (ReaderT, runReaderT)
import           Data.Aeson                           (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy.Char8           as LC8
import           Data.Generics.Product                (field)
import           Data.Text                            (Text)
import           Database.SQLite.SimpleErrors.Types   (SQLiteResponse)
import           GHC.Generics                         (Generic)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.Cors          (cors, corsOrigins, corsRequestHeaders,
                                                       simpleCorsResourcePolicy)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant
import           Servant.Auth.Server

import Game.Dissidence.Config (load)
import Game.Dissidence.Db     (AsDbError (..), AsDbLogicError (..), AsSQLiteResponse (..), ChatLine,
                               DbConstraints, DbError (..), DbGameState, DbUser, GameId (..), NewChatLine,
                               Posix, checkLogin, initDb, insertChatLine, insertUser, selectChatLines,
                               selectGameState)
import Game.Dissidence.Env    (Env, EnvWConnection, configToEnv, envCookieSettings, envDbPath, envJwtSettings,
                               withEnvDbConnection)

data AppError = AppLoginFailed | AppRequireUser | AppDbError DbError deriving (Show, Generic)
makeClassyPrisms ''AppError

instance AsDbError AppError where
  _DbError = _AppDbError . _DbError

instance AsDbLogicError AppError where
  _DbLogicError = _AppDbError . _DbLogicError

instance AsSQLiteResponse AppError where
  _SQLiteResponse = _AppDbError . _SQLiteResponse

class HasCookieSettings a where
  cookieSettings :: Getter a CookieSettings

class HasJwtSettings a where
  jwtSettings :: Getter a JWTSettings

instance HasCookieSettings EnvWConnection where
  cookieSettings = envCookieSettings

instance HasJwtSettings EnvWConnection where
  jwtSettings = envJwtSettings

type AppConstraints e r m = (DbConstraints e r m, HasCookieSettings r, HasJwtSettings r, AsAppError e)

type ChatApi
  =    QueryParam "since" Posix :> Get '[JSON] [ChatLine]
  :<|> ReqBody '[JSON] NewChatLine :> Post '[JSON] ()

type GameApi = Get '[JSON] DbGameState

type UserApi
  =    ReqBody '[JSON] DbUser :> Post '[JSON] ()

type LoginApi
  = ReqBody '[JSON] DbUser :> PostNoContent '[JSON]
    (Headers '[ Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] ())

type Api = "api" :>
  ("lobby" :> ChatApi
  :<|> "game" :> GameApi
  :<|> "user" :> UserApi
  :<|> "login" :> LoginApi
  )

data Session = Session { username :: Text } deriving Generic
instance ToJSON Session
instance FromJSON Session
instance ToJWT Session
instance FromJWT Session

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
loginPost
  :: AppConstraints e r m
  => DbUser
  -> m (Headers '[Header "Set-Cookie" SetCookie, Header "Set-Cookie" SetCookie] ())
loginPost dbUser = do
  cookieS <- view cookieSettings
  jwtS <- view jwtSettings
  b <- checkLogin dbUser
  unless b $ throwing _AppRequireUser ()
  mApplyCookies <- liftIO $ acceptLogin cookieS jwtS (Session (dbUser ^. field @"dbUsername"))
  case mApplyCookies of
    Nothing         -> throwing _AppRequireUser ()
    Just appCookies -> pure $ appCookies ()


app :: Env -> Application
app e = serve api (hoistServer api (appHandler e) server)

runDbContext :: (AsSQLiteResponse e, MonadIO m) => Env -> ExceptT e (ReaderT EnvWConnection IO) a -> m (Either e a)
runDbContext e = liftIO . withEnvDbConnection e . runReaderT . runExceptT

appHandler :: Env -> ExceptT AppError (ReaderT EnvWConnection IO) a -> Handler a
appHandler e prog = do
  res <- runDbContext e prog
  case res of
    Left (AppDbError err) -> throwError $ err500 { errBody = "Database error: " <> LC8.pack (show err) }
    Left AppLoginFailed   -> throwError err403
    Left AppRequireUser   -> throwError err401
    Right a               -> pure a

runApp :: IO ()
runApp = do
  c  <- load "./config" -- TODO: Opts for path
  e  <- configToEnv c
  putStrLn $ "Opening/Initialising sqlite database " <> (e ^.envDbPath)
  initRes <- runDbContext e initDb
  case initRes of
    Left (err ::SQLiteResponse) -> error $ "DB Failed to initialise: " <> (show err)
    Right _  -> do
      let port = c ^. field @"port" . to fromIntegral
      putStrLn $ "Starting server on port " <> (show port)
      let corsPolicy = simpleCorsResourcePolicy
            { corsRequestHeaders = ["content-type"]
            , corsOrigins = Just (["http://localhost:1234"], True)
            }
      run port . cors (const (Just corsPolicy)) . logStdoutDev . app $ e
