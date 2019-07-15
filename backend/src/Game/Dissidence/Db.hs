{-# LANGUAGE ConstraintKinds, DataKinds, DeriveGeneric, FlexibleContexts, NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TemplateHaskell, TypeApplications   #-}
module Game.Dissidence.Db
  ( initDb
  , DbConstraints
  , selectChatLines
  , insertChatLine
  , selectGameState
  , insertGameState
  , updateGameState
  , insertUser
  , checkLogin
--  , insertUser
--  , upsertGameState
--  , selectUser
 -- , selectGameState
  , runDb
  , ChatLine
  , NewChatLine
  , DbGameState
  , DbUser
  , AsSQLiteResponse(..)
  , AsDbError(..)
  , AsDbLogicError(..)
  , GameId(..)
  , DbError(..)
  , Posix
  ) where

import Control.Lens

import           Control.Exception                  (throw)
import           Control.Lens.Cons                  (_head)
import           Control.Monad                      ((<=<))
import           Control.Monad                      (unless)
import           Control.Monad.Error.Lens           (throwing)
import           Control.Monad.Except               (MonadError)
import           Control.Monad.IO.Class             (MonadIO, liftIO)
import           Control.Monad.Reader               (MonadReader, ask)
import           Crypto.PasswordStore               (makePassword, verifyPassword)
import           Data.Aeson                         (FromJSON, ToJSON, eitherDecode, encode)
import           Data.Foldable                      (traverse_)
import qualified Data.Generics.Product              as GP
import           Data.Maybe                         (isNothing)
import           Data.Text                          (Text)
import           Data.Text.Encoding                 (decodeUtf8, encodeUtf8)
import           Database.SQLite.Simple             (Connection, FromRow (..), Only (..), Query,
                                                     ResultError (ConversionFailed), SQLData, ToRow (..),
                                                     execute, execute_, field, lastInsertRowId, query, query_)
import           Database.SQLite.Simple.FromField   (FromField (..))
import           Database.SQLite.Simple.Internal    (RowParser)
import           Database.SQLite.Simple.ToField     (ToField (..))
import           Database.SQLite.SimpleErrors       (runDBAction)
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)
import           GHC.Generics                       (Generic)
import           Servant.Elm                        (deriveBoth)

import Game.Dissidence.AesonOptions (ourAesonOptions)
import Game.Dissidence.GameState    (GameState)

toAesonField :: ToJSON a => a -> SQLData
toAesonField = toField . encode

aesonField :: FromJSON a => RowParser a
aesonField = field >>= (either (throw . (ConversionFailed "TEXT" "Jsony Blob") . show) pure . eitherDecode)

type Posix = Integer

newtype GameId = GameId { unGameId :: Integer } deriving (Eq, Ord, Show, Generic)

instance ToField GameId where
  toField = toField . unGameId

instance FromField GameId where
  fromField = fmap GameId . fromField

data ChatLine = ChatLine
  { chatLineTime     :: Posix
  , chatLineGameId   :: Maybe GameId
  , chatLineUsername :: Text
  , chatLineText     :: Text
  } deriving (Show, Generic)

instance FromRow ChatLine where
  fromRow = ChatLine <$> field <*> field <*> field <*> field

data NewChatLine = NewChatLine
  { newChatGameId       :: Maybe GameId
  , newChatLineUsername :: Text
  , newChatLineText     :: Text
  } deriving (Show, Generic)

instance ToRow NewChatLine where
  toRow (NewChatLine gId u t) = [toField gId, toField u, toField t]

data DbGameState = DbGameState
  { dbGameStateId :: GameId
  , dbGameState   :: GameState
  } deriving (Show, Generic)

instance FromRow DbGameState where
  fromRow = DbGameState <$> (GameId <$> field) <*> aesonField

newtype NewDbGameState = NewDbGameState { unNewDbGameState :: GameState } deriving (Show, Generic)

instance ToRow NewDbGameState where
  toRow (NewDbGameState gs) = [toAesonField gs]

data DbUser = DbUser
  { dbUsername     :: Text
  , dbUserPassword :: Text
  } deriving (Show, Generic)

instance FromRow DbUser where
  fromRow = DbUser <$> field <*> field

instance ToRow DbUser where
  toRow (DbUser u p) = [toField u, toField p]

concat <$> mapM
  (deriveBoth ourAesonOptions)
  [''GameId, ''DbGameState, ''NewDbGameState, ''ChatLine, ''NewChatLine, ''DbUser]

class HasConnection s where
  connection :: Lens' s Connection

instance HasConnection Connection where
  connection = lens id (const id)

data DbLogicError = UserAlreadyExists Text | GameDoesntExist GameId deriving (Eq, Show, Generic)
makeClassyPrisms ''DbLogicError

makeClassyPrisms ''SQLiteResponse

data DbError = DbErrorDbLogicError DbLogicError | DbErrorSQLiteResponse SQLiteResponse deriving (Eq, Show, Generic)
makeClassyPrisms ''DbError

instance AsDbLogicError DbError where
  _DbLogicError = _DbErrorDbLogicError . _DbLogicError

instance AsSQLiteResponse DbError where
  _SQLiteResponse = _DbErrorSQLiteResponse . _SQLiteResponse

type SqLiteConstraints e r m =
  ( MonadReader r m
  , HasConnection r
  , MonadIO m
  , AsSQLiteResponse e
  , MonadError e m
  )

type DbConstraints e r m = (SqLiteConstraints e r m, AsDbLogicError e)

selectChatLines :: SqLiteConstraints e r m => Maybe GameId -> m [ChatLine]
selectChatLines Nothing = query_' "SELECT epoch, game_id, username, text FROM chat_line"
selectChatLines (Just gId) = query' "SELECT epoch, game_id, username, text FROM chat_line WHERE game_id = ?" (Only gId)

insertChatLine :: DbConstraints e r m => NewChatLine -> m ()
insertChatLine = execute' "INSERT INTO chat_line (game_id,username, text) VALUES (?,?,?)"

selectGameState :: DbConstraints e r m => GameId -> m DbGameState
selectGameState gId = do
  gs <- query' "SELECT id, data FROM game_state WHERE id = ?" (Only gId)
  maybe (throwing _GameDoesntExist gId) pure (gs^?_head)

updateGameState :: DbConstraints e r m => DbGameState -> m ()
updateGameState (DbGameState gId d)= execute' "UPDATE game_state SET data = ? WHERE id = ?" (toAesonField d, gId)

insertGameState :: DbConstraints e r m => NewDbGameState -> m GameId
insertGameState = fmap GameId . insert "INSERT INTO game_state (data) VALUES (?)"

findUser :: DbConstraints e r m => Text -> m (Maybe DbUser)
findUser = fmap (^?_head) . query' "SELECT username, password FROM user WHERE username = ?" . Only

insertUser :: DbConstraints e r m => DbUser -> m ()
insertUser (DbUser uname p) = do
  uMay <- findUser uname
  unless (isNothing uMay) $ throwing _UserAlreadyExists uname
  pwBs <- liftIO $ makePassword (encodeUtf8 p) 17
  execute' "INSERT INTO user (username, password) VALUES (?,?)" (uname,(decodeUtf8 pwBs))

checkLogin :: DbConstraints e r m => DbUser -> m Bool
checkLogin (DbUser u p) = do
  uMay <- findUser u
  pure $ maybe False (verifyPassword (encodeUtf8 p) . (^.GP.field @"dbUserPassword".to encodeUtf8)) uMay

initDb ::
  SqLiteConstraints e r m
  => m ()
initDb =
  let
    enableForeignKeys = "PRAGMA foreign_keys = ON;"
    qGameState = "CREATE TABLE IF NOT EXISTS game_state\
      \( id INTEGER PRIMARY KEY\
      \, epoch INTEGER NOT NULL DEFAULT (strftime('%s',CURRENT_TIMESTAMP))\
      \, data TEXT NOT NULL\
      \)"
    qChatLines = "CREATE TABLE IF NOT EXISTS chat_line\
      \( id INTEGER PRIMARY KEY\
      \, epoch INTEGER NOT NULL DEFAULT (strftime('%s',CURRENT_TIMESTAMP))\
      \, game_id INTEGER REFERENCES game_state(id)\
      \, username TEXT REFERENCES user(id)\
      \, text TEXT NOT NULL\
      \)"
    qUsers = "CREATE TABLE IF NOT EXISTS user\
      \( username TEXT NOT NULL PRIMARY KEY \
      \, password TEXT NOT NULL\
      \)"
  in
    withConnIO $ \conn ->
      traverse_ (execute_ conn)
        [ enableForeignKeys
        , qGameState
        , qChatLines
        , qUsers
        ]

insert ::
  ( SqLiteConstraints e r m
  , ToRow a
  )
  => Query
  -> a
  -> m Integer
insert q a =
  withConnIO $ \conn -> do
    execute conn q a
    fromIntegral <$> lastInsertRowId conn

query_' :: (SqLiteConstraints e r m, FromRow a) => Query -> m [a]
query_' q = withConnIO (\c -> query_ c q)

query' :: (SqLiteConstraints e r m, FromRow a, ToRow i) => Query -> i -> m [a]
query' q i = withConnIO (\c -> query c q i)

execute' :: (SqLiteConstraints e r m, ToRow i) => Query -> i -> m ()
execute' q i = withConnIO (\c -> execute c q i)

withConnIO ::
  SqLiteConstraints e r m
  => (Connection -> IO a)
  -> m a
withConnIO f =
  (runDb . f . view connection) =<< ask

runDb ::
  ( MonadError e m
  , AsSQLiteResponse e
  , MonadIO m
  )
  => IO a
  -> m a
runDb =
  either (throwing _SQLiteResponse) pure <=< liftIO . runDBAction
