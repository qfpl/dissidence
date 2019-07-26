{-# LANGUAGE ConstraintKinds, DataKinds, DeriveGeneric, FlexibleContexts, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TypeApplications, TypeOperators          #-}
module Game.Dissidence where

import Control.Lens hiding (Context)

import           Control.Monad                        (unless)
import           Control.Monad.Error.Lens             (throwing)
import           Control.Monad.Except                 (ExceptT, MonadError, runExceptT)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.Reader                 (ReaderT, runReaderT)
import           Crypto.JOSE                          (Error)
import           Data.Aeson                           (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy.Char8           as LC8
import           Data.Generics.Product                (field)
import           Data.Text                            (Text)
import qualified Data.Text.Lazy                       as TL
import qualified Data.Text.Lazy.Encoding              as TL
import           Database.SQLite.SimpleErrors.Types   (SQLiteResponse)
import           GHC.Generics                         (Generic)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.Cors          (cors, corsOrigins, corsRequestHeaders,
                                                       simpleCorsResourcePolicy)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant
import           Servant.Auth.Server

import Game.Dissidence.Config    (load)
import Game.Dissidence.Db        (AsDbError (..), AsDbLogicError (..), AsSQLiteResponse (..), ChatLine,
                                  DbConstraints, DbError (..), DbGameState, DbUser, GameId (..), JoinableGame,
                                  NewChatLine (..), NewDbGameState (..), Posix, checkLogin, initDb,
                                  insertChatLine, insertGameState, insertUser, listJoinableGames,
                                  selectChatLines, selectGameState)
import Game.Dissidence.Env       (Env, EnvWConnection, configToEnv, envCookieSettings, envDbPath,
                                  envJwtSettings, withEnvDbConnection)
import Game.Dissidence.GameState (PlayerId (..), newGame)

data AppError = AppJwtMisconfiguration Error | AppLoginFailed | AppRequireUser | AppDbError DbError deriving (Show, Generic)
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

instance HasCookieSettings Env where
  cookieSettings = envCookieSettings

instance HasJwtSettings Env where
  jwtSettings = envJwtSettings

instance HasCookieSettings EnvWConnection where
  cookieSettings = envCookieSettings

instance HasJwtSettings EnvWConnection where
  jwtSettings = envJwtSettings

type AppConstraints e r m = (DbConstraints e r m, HasCookieSettings r, HasJwtSettings r, AsAppError e)

type ChatApi = Auth '[JWT] Session :> AuthedChatApi

type AuthedChatApi
  =    QueryParam "since" Posix :> Get '[JSON] [ChatLine]
  :<|> ReqBody '[JSON] Text :> Post '[JSON] ()

type GameApi
  = Auth '[JWT] Session :>
    ( Capture "gameId" GameId :> (Get '[JSON] DbGameState :<|> "chat" :> AuthedChatApi)
    :<|> Post '[JSON] GameId
    :<|> "joinable" :> Get '[JSON] [JoinableGame]
    )

type UserApi
  =    ReqBody '[JSON] DbUser :> Post '[JSON] String

type LoginApi
  = ReqBody '[JSON] DbUser :> Post '[JSON] String

type Api = "api" :>
  ("lobby" :> ChatApi
  :<|> "games" :> GameApi
  :<|> "users" :> UserApi
  :<|> "login" :> LoginApi
  )

data Session = Session { username :: Text } deriving Generic
instance ToJSON Session
instance FromJSON Session
instance ToJWT Session
instance FromJWT Session

sessionToJwt :: (MonadError e m, AsAppError e, MonadIO m) => JWTSettings -> Session -> m String
sessionToJwt jwtS s = do
  res <- liftIO $ makeJWT s jwtS Nothing
  case res of
    Left e  -> throwing _AppJwtMisconfiguration e
    -- Yeah, this is cheating I know. Servant-elm can't tack on the bearer bit. :(
    Right r -> pure . ("Bearer " <>) . TL.unpack . TL.decodeUtf8 $ r

api :: Proxy Api
api = Proxy

server :: AppConstraints e r m => ServerT Api m
server = chatApi Nothing :<|> gameApi :<|> userPost :<|> loginPost

reqUser :: (MonadError e m, AsAppError e) => m a
reqUser = throwing _AppRequireUser ()

gameApi :: (AppConstraints e r m) => ServerT GameApi m
gameApi (Authenticated s) = (\gId -> gameGet s gId :<|> authedChatApi s (Just gId)) :<|> gameNew s :<|> joinableGames s
gameApi _                 = (const (reqUser :<|> (const reqUser :<|> const reqUser))) :<|> reqUser :<|> reqUser

chatApi :: (AppConstraints e r m) => Maybe GameId -> ServerT ChatApi m
chatApi gIdMay (Authenticated s) = authedChatApi s gIdMay
chatApi _ _                      = (const reqUser) :<|> (const reqUser)

authedChatApi :: (AppConstraints e r m) => Session -> Maybe GameId -> ServerT AuthedChatApi m
authedChatApi s gIdMay = chatGet s gIdMay :<|> chatAppend s gIdMay

chatGet :: AppConstraints e r m => Session -> Maybe GameId -> Maybe Posix -> m [ChatLine]
chatGet _ gIdMay p = selectChatLines p gIdMay

joinableGames :: AppConstraints e r m => Session -> m [JoinableGame]
joinableGames _ = listJoinableGames

chatAppend :: AppConstraints e r m => Session -> Maybe GameId -> Text -> m ()
chatAppend s gIdMay t = insertChatLine (NewChatLine gIdMay (username s) t)

gameGet :: AppConstraints e r m => Session -> GameId -> m DbGameState
gameGet _ gId = selectGameState gId

gameNew :: AppConstraints e r m => Session -> m GameId
gameNew s = insertGameState (NewDbGameState (newGame (PlayerId (username s))))

userPost :: AppConstraints e r m => DbUser -> m String
userPost dbUser = do
  jwtS <- view jwtSettings
  insertUser dbUser
  sessionToJwt jwtS (Session (dbUser ^. field @"dbUsername"))

loginPost :: AppConstraints e r m => DbUser -> m String
loginPost dbUser = do
  jwtS <- view jwtSettings
  b <- checkLogin dbUser
  unless b $ throwing _AppRequireUser ()
  sessionToJwt jwtS (Session (dbUser ^. field @"dbUsername"))

app :: Env -> Application
app e = serveWithContext api ctx (hoistServerWithContext api (Proxy :: Proxy '[CookieSettings, JWTSettings]) (appHandler e) server)
  where
    ctx = (e ^. cookieSettings) :. (e ^. jwtSettings) :. EmptyContext

runDbContext :: (AsSQLiteResponse e, MonadIO m) => Env -> ExceptT e (ReaderT EnvWConnection IO) a -> m (Either e a)
runDbContext e = liftIO . withEnvDbConnection e . runReaderT . runExceptT

appHandler :: Env -> ExceptT AppError (ReaderT EnvWConnection IO) a -> Handler a
appHandler e prog = do
  res <- runDbContext e prog
  case res of
    Left (AppDbError err)            -> throwError $ err500 { errBody = "Database error: " <> LC8.pack (show err) }
    Left AppLoginFailed              -> throwError err403
    Left AppRequireUser              -> throwError err401
    Left (AppJwtMisconfiguration je) -> throwError $ err500 { errBody = "JWT Misconfigured " <> LC8.pack (show je)}
    Right a                          -> pure a

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
            { corsRequestHeaders = ["content-type","authorization"]
            , corsOrigins = Just (["http://localhost:1234"], True)
            }
      run port . cors (const (Just corsPolicy)) . logStdoutDev . app $ e
