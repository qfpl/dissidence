{-# LANGUAGE DataKinds, DeriveGeneric, OverloadedStrings, ScopedTypeVariables, TemplateHaskell #-}
{-# LANGUAGE TypeApplications, TypeOperators                                                   #-}
module Game.Dissidence where

import Control.Lens hiding (use)

import           Control.Monad.IO.Class     (liftIO)
import           Data.Aeson                 (FromJSON, ToJSON)
import qualified Data.ByteString.Lazy       as LBS
import           Data.Functor.Contravariant ((>$<))
import           Data.Generics.Product      (field)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Encoding         (encodeUtf8)
import           Data.Time                  (UTCTime)
import           Data.Vector                (Vector)
import           GHC.Generics               (Generic)
import qualified Hasql.Decoders             as D
import qualified Hasql.Encoders             as E
import           Hasql.Migration            (MigrationCommand (..), MigrationError,
                                             loadMigrationsFromDirectory, runMigration)
import           Hasql.Pool                 (UsageError, use)
import           Hasql.Statement            (Statement (Statement))
import           Hasql.Transaction          (Transaction, statement)
import           Hasql.Transaction.Sessions (IsolationLevel (Serializable), Mode (Read, Write), transaction)
import           Network.Wai.Handler.Warp   (run)
import           Servant
import           Servant.Elm                (defaultOptions, deriveBoth)

import Game.Dissidence.Config (Config, load)
import Game.Dissidence.Env    (Env, configToEnv)

data ChatLine = ChatLine
  { chatLineTime     :: UTCTime
  , chatLineUsername :: Text
  , chatLineText     :: Text
  } deriving (Show, Generic)

data NewChatLine = NewChatLine
  { newChatLineUsername :: Text
  , newChatLineText     :: Text
  } deriving (Show, Generic)

type ChatApi =
  QueryParam "since" UTCTime :> Get '[JSON] (Vector ChatLine)
  :<|> ReqBody '[JSON] NewChatLine :> Post '[JSON] NoContent

concat <$> mapM
  (deriveBoth defaultOptions)
  [''ChatLine, ''NewChatLine]

type Api = "api" :> "lobby" :> ChatApi

api :: Proxy Api
api = Proxy

server :: ServerT Api Transaction
server = globalChatGet :<|> globalChatAppend

globalChatGet    _ = statement () $ Statement "SELECT time, username, msg FROM chatlog" E.unit decoder False
  where
    decoder = D.rowVector $ ChatLine
      <$> D.column D.timestamptz
      <*> D.column D.text
      <*> D.column D.text

globalChatAppend nl = statement nl $ Statement
  "INSERT INTO chatlog (time, username, msg) VALUES (NOW(),$1,$2)"
  encoder
  (NoContent <$ D.unit)
  True
  where
    encoder =
      (newChatLineUsername >$< E.param E.text) <>
      (newChatLineText >$< E.param E.text)


runTransaction :: Env -> Transaction a -> IO (Either UsageError a)
runTransaction e t = use (e ^. field @"dbPool") . transaction Serializable Write $ t

transactionHandler :: Env -> Transaction a -> Handler a
transactionHandler e t = do
  dbRes <- liftIO $ runTransaction e t
  either (throwError . usageErrorToServantErr) pure dbRes
  where
    usageErrorToServantErr ue = err500 { errBody = LBS.fromStrict . encodeUtf8 . T.pack . show $ ue }

app :: Env -> Application
app e = serve api (hoistServer api (transactionHandler e) server)

runApp :: IO ()
runApp = do
  c  <- load "./config" -- TODO: Opts for path
  e  <- configToEnv c
  ms <- loadMigrationsFromDirectory $ "./migrations/"
  mRes <- fmap (fmap (^? traverse . _Just)) . runTransaction e . traverse runMigration $ MigrationInitialization : ms
  case mRes of
    Left ue         -> error $ "Error running migrations: " <> show ue
    Right (Just me) -> error $ "Error running migrations: " <> show me
    Right Nothing   -> run 8001 (app e)
