{-# LANGUAGE ConstraintKinds, DataKinds, DeriveFunctor, DeriveGeneric, FlexibleContexts                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell, TypeApplications, TypeOperators                                          #-}
module Game.Dissidence where

import Control.Lens hiding (Context)

import           Control.Monad                        (unless, when)
import           Control.Monad.Error.Lens             (throwing)
import           Control.Monad.Except                 (ExceptT, MonadError, runExceptT)
import           Control.Monad.IO.Class               (MonadIO, liftIO)
import           Control.Monad.Reader                 (MonadReader, ReaderT, runReaderT)
import           Crypto.JOSE                          (Error)
import           Data.Aeson                           (FromJSON, ToJSON, encode)
import qualified Data.ByteString.Lazy.Char8           as LC8
import           Data.Generics.Product                (field)
import           Data.Maybe                           (fromMaybe, isNothing)
import           Data.Random                          ()
import           Data.Random.Internal.Source          (MonadRandom (getRandomPrim))
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.Text.Lazy                       as TL
import qualified Data.Text.Lazy.Encoding              as TL
import           GHC.Generics                         (Generic)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.Cors          (cors, corsOrigins, corsRequestHeaders,
                                                       simpleCorsResourcePolicy)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Safe                                 (headMay)
import           Servant
import           Servant.Auth.Server
import           System.Environment                   (getArgs)

import Game.Dissidence.Config    (load)
import Game.Dissidence.Db        (AsDbError (..), AsDbLogicError (..), AsSQLiteResponse (..), ChatLine,
                                  DbConstraints, DbError (..), DbLogicError (..), DbPlayer, GameId (..),
                                  JoinableGame, NewChatLine (..), NewDbGameState (..),
                                  NewDbGameStateEvent (..), Posix, checkLogin, findPlayer, initDb,
                                  insertChatLine, insertGameState, insertGameStateEvent, insertPlayer,
                                  listGameStateEvents, listJoinableGames, selectChatLines, selectGameState,
                                  updateGameState)
import Game.Dissidence.Env       (Env, EnvWConnection, configToEnv, envCookieSettings, envDbPath,
                                  envJwtSettings, withEnvDbConnection)
import Game.Dissidence.GameState (AsGameStateInputError (..), GameStateInputError, PlayerId, inputEvent,
                                  newGame)
import Game.Dissidence.ViewState (DbViewState, NewViewStateEvent (..), ViewStateEvent, gameEventsToViewEvents,
                                  gameStateToViewState)

data AppError
  = AppJwtMisconfiguration Error
  | AppLoginFailed
  | AppRequirePlayer
  | AppForbidden
  | AppDbError DbError
  | AppGameStateInputError GameStateInputError
  deriving (Show, Generic)
makeClassyPrisms ''AppError

instance AsDbError AppError where
  _DbError = _AppDbError . _DbError

instance AsDbLogicError AppError where
  _DbLogicError = _AppDbError . _DbLogicError

instance AsSQLiteResponse AppError where
  _SQLiteResponse = _AppDbError . _SQLiteResponse

instance AsGameStateInputError AppError where
  _GameStateInputError = _AppGameStateInputError . _GameStateInputError

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

type AppConstraints e r m = (DbConstraints e r m, HasCookieSettings r, HasJwtSettings r, AsAppError e, MonadRandom m, AsGameStateInputError e)

newtype AppM' e a = AppM { unAppM :: ExceptT e (ReaderT EnvWConnection IO) a } deriving (MonadError e, MonadReader EnvWConnection, Functor, Applicative, Monad, MonadIO)
type AppM = AppM' AppError

runAppM' :: AppM' e a -> EnvWConnection -> IO (Either e a)
runAppM' m e = flip runReaderT e . runExceptT . unAppM $ m

instance MonadRandom (AppM' e) where
  getRandomPrim = liftIO . getRandomPrim

data Session = Session { playerId :: PlayerId } deriving Generic
instance ToJSON Session
instance FromJSON Session
instance ToJWT Session
instance FromJWT Session

type ChatApi = Auth '[JWT] Session :>
  (    QueryParam "since" Posix :> Get '[JSON] [ChatLine]
  :<|> ReqBody '[JSON] Text :> Post '[JSON] ()
  )

type GameApi
  = Auth '[JWT] Session :>
    ( Capture "gameId" GameId :>
      (    Get '[JSON] DbViewState
      :<|> "events" :>
        (    QueryParam "since" Posix :> Get '[JSON] [ViewStateEvent]
        :<|> ReqBody '[JSON] NewViewStateEvent :> Post '[JSON] ()
        )
      )
    :<|> Post '[JSON] GameId
    :<|> "joinable" :> Get '[JSON] [JoinableGame]
    )

type PlayerApi
  =    ReqBody '[JSON] DbPlayer :> Post '[JSON] String

type LoginApi
  = ReqBody '[JSON] DbPlayer :> Post '[JSON] String

type Api = "api" :>
  ("lobby" :> ChatApi
  :<|> "games" :> GameApi
  :<|> "players" :> PlayerApi
  :<|> "login" :> LoginApi
  )


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
server = chatApi Nothing :<|> gameApi :<|> playerPost :<|> loginPost

type AuthRes = AuthResult Session

-- This doesn't have an expiry in it, does it? Probs should fix. :)
checkSession :: AppConstraints e r m => AuthRes -> m Session
checkSession (Authenticated s) = do
  pMay <- findPlayer (s ^. field @"playerId")
  when (isNothing pMay) $ throwing _AppRequirePlayer ()
  pure s
checkSession _ = throwing _AppRequirePlayer ()

gameApi :: (AppConstraints e r m) => ServerT GameApi m
gameApi authRes =
  (\gId -> gameGet authRes gId
    :<|> ( gameLogs authRes gId :<|> gameUpdate authRes gId )
    )
  :<|> gameNew authRes
  :<|> joinableGames authRes

chatApi :: (AppConstraints e r m) => Maybe GameId -> ServerT ChatApi m
chatApi gIdMay authRes = chatGet authRes gIdMay :<|> chatAppend authRes gIdMay

chatGet :: AppConstraints e r m => AuthRes -> Maybe GameId -> Maybe Posix -> m [ChatLine]
chatGet authRes gIdMay p = checkSession authRes *> selectChatLines p gIdMay

joinableGames :: AppConstraints e r m => AuthRes -> m [JoinableGame]
joinableGames authRes = checkSession authRes *> listJoinableGames

chatAppend :: AppConstraints e r m => AuthRes -> Maybe GameId -> Text -> m ()
chatAppend authRes gIdMay t = do
  (Session pId) <- checkSession authRes
  insertChatLine (NewChatLine gIdMay pId t)

gameGet :: AppConstraints e r m => AuthRes -> GameId -> m DbViewState
gameGet authRes gId = do
  (Session pId) <- checkSession authRes
  dbGs <- selectGameState gId
  gameStateToViewState pId dbGs

gameUpdate :: AppConstraints e r m => AuthRes -> GameId -> NewViewStateEvent -> m ()
gameUpdate authRes gId inputE  = do
  (Session pId) <- checkSession authRes
  -- TODO THIS NEEDS A TRANSACTTION!!!
  gs <- selectGameState gId
  case inputE of
    NewViewStateEventChat nct -> do
      insertChatLine (NewChatLine (Just gId) pId nct)
    NewViewStateEventInput gsi -> do
      (nGs,internalE,outputE) <- inputEvent (gs ^. field @"dbGameState") pId gsi Nothing
      updateGameState (gs & field @"dbGameState" .~ nGs)
      insertGameStateEvent (NewDbGameStateEvent gId pId gsi internalE outputE)

gameLogs :: AppConstraints e r m => AuthRes -> GameId -> Maybe Posix -> m [ViewStateEvent]
gameLogs authRes gId sinceMay = do
  (Session pId) <- checkSession authRes
  gses <- listGameStateEvents gId sinceMay
  cls  <- selectChatLines sinceMay (Just gId)
  gameEventsToViewEvents pId cls gses

gameNew :: AppConstraints e r m => AuthRes -> m GameId
gameNew authRes = do
  (Session pId) <- checkSession authRes
  insertGameState (NewDbGameState (newGame pId))

playerPost :: AppConstraints e r m => DbPlayer -> m String
playerPost dbPlayer = do
  jwtS <- view jwtSettings
  insertPlayer dbPlayer
  sessionToJwt jwtS (Session (dbPlayer ^. field @"dbPlayerId"))

loginPost :: AppConstraints e r m => DbPlayer -> m String
loginPost dbPlayer = do
  jwtS <- view jwtSettings
  b <- checkLogin dbPlayer
  unless b $ throwing _AppRequirePlayer ()
  sessionToJwt jwtS (Session (dbPlayer ^. field @"dbPlayerId"))

app :: Env -> Application
app e = serveWithContext api ctx (hoistServerWithContext api (Proxy :: Proxy '[CookieSettings, JWTSettings]) (appHandler e) server)
  where
    ctx = (e ^. cookieSettings) :. (e ^. jwtSettings) :. EmptyContext

runDbContext :: (AsSQLiteResponse e, MonadIO m) => Env -> AppM' e a -> m (Either e a)
runDbContext e = liftIO . withEnvDbConnection e . runAppM'

appHandler :: Env -> AppM a -> Handler a
appHandler e prog = do
  res <- runDbContext e prog
  case res of
    Left (AppDbError (DbErrorSQLiteResponse err)) -> throwError $ err500 { errBody = "Database error: " <> LC8.pack (show err) }
    Left (AppDbError (DbErrorDbLogicError err)) -> case err of
      (PlayerAlreadyExists _pId) -> throwError err400 { errBody = "Player already exists" }
      (GameDoesntExist _gId)     -> throwError err404
    Left AppLoginFailed              -> throwError err403
    Left AppRequirePlayer            -> throwError err401
    Left AppForbidden                -> throwError err403
    Left (AppJwtMisconfiguration je) -> throwError $ err500 { errBody = "JWT Misconfigured " <> LC8.pack (show je)}
    Left (AppGameStateInputError ie) -> throwError $ err400 { errBody = encode ie }
    Right a                          -> pure a

runApp :: IO ()
runApp = do
  args <- getArgs
  let path = fromMaybe "./config" $ headMay args
  c  <- load (T.pack path)
  e  <- configToEnv c
  putStrLn $ "Opening/Initialising sqlite database " <> (e ^.envDbPath)
  initRes <- runDbContext e initDb
  case initRes of
    Left (err :: DbError) -> error $ "DB Failed to initialise: " <> (show err)
    Right _  -> do
      let port = c ^. field @"port" . to fromIntegral
      putStrLn $ "Starting server on port " <> (show port)
      let corsPolicy = simpleCorsResourcePolicy
            { corsRequestHeaders = ["content-type","authorization"]
            , corsOrigins = Just (["http://localhost:1234","http://localhost:1235"], True)
            }
      run port . cors (const (Just corsPolicy)) . logStdoutDev . app $ e
