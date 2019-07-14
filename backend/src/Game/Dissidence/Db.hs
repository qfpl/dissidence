{-# LANGUAGE ConstraintKinds, DataKinds, DeriveGeneric, FlexibleContexts, NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TemplateHaskell, TypeApplications   #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds #-}

module Game.Dissidence.Db
  ( initDb
  , DbConstraints
  , selectChatLines
  , insertChatLine
  , selectGameState
  , insertGameState
  , updateGameState
--  , insertUser
--  , upsertGameState
--  , selectUser
 -- , selectGameState
  , runDb
  , ChatLine
  , NewChatLine
  , DbGameState
  , AsSQLiteResponse(..)
  , GameId(..)
  , Posix
  ) where

import Control.Lens

import Control.Exception                  (throw)
import Control.Lens.Cons                  (_head)
import Control.Monad                      ((<=<))
import Control.Monad.Error.Lens           (throwing)
import Control.Monad.Except               (MonadError)
import Control.Monad.IO.Class             (MonadIO, liftIO)
import Control.Monad.Reader               (MonadReader, ask)
import Data.Aeson                         (FromJSON, ToJSON, eitherDecode, encode)
import Data.Foldable                      (traverse_)
import Data.Text                          (Text)
import Database.SQLite.Simple             (Connection, FromRow (..), Only (..), Query,
                                           ResultError (ConversionFailed), SQLData, ToRow (..), execute,
                                           execute_, field, lastInsertRowId, query, query_)
import Database.SQLite.Simple.FromField   (FromField (..))
import Database.SQLite.Simple.Internal    (RowParser)
import Database.SQLite.Simple.ToField     (ToField (..))
import Database.SQLite.SimpleErrors       (runDBAction)
import Database.SQLite.SimpleErrors.Types (SQLiteResponse)
import GHC.Generics                       (Generic)
import Servant.Elm                        (deriveBoth)

import Game.Dissidence.AesonOptions (ourAesonOptions)
import Game.Dissidence.GameState    (GameState, PlayerId (PlayerId), newGame)

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

concat <$> mapM
  (deriveBoth ourAesonOptions)
  [''GameId, ''DbGameState, ''NewDbGameState, ''ChatLine, ''NewChatLine]

class HasConnection s where
  connection :: Lens' s Connection

instance HasConnection Connection where
  connection = lens id (const id)

makeClassyPrisms ''SQLiteResponse

type DbConstraints e r m =
  ( MonadReader r m
  , HasConnection r
  , MonadIO m
  , AsSQLiteResponse e
  , MonadError e m
  )

selectChatLines :: DbConstraints e r m => Maybe GameId -> m [ChatLine]
selectChatLines Nothing = query_' "SELECT epoch, game_id, username, text FROM chat_line"
selectChatLines (Just gId) = query' "SELECT epoch, game_id, username, text FROM chat_line WHERE game_id = ?" (Only gId)

insertChatLine :: DbConstraints e r m => NewChatLine -> m ()
insertChatLine = execute' "INSERT INTO chat_line (game_id,username, text) VALUES (?,?,?)"

selectGameState :: DbConstraints e r m => GameId -> m (Maybe DbGameState)
selectGameState = fmap (^?_head) . query' "SELECT id, data FROM game_state WHERE id = ?" . Only

updateGameState :: DbConstraints e r m => DbGameState -> m ()
updateGameState (DbGameState gId d)= execute' "UPDATE game_state SET data = ? WHERE id = ?" (toAesonField d, gId)

insertGameState :: DbConstraints e r m => NewDbGameState -> m GameId
insertGameState = fmap GameId . insert "INSERT INTO game_state (data) VALUES (?)"

initDb ::
  DbConstraints e r m
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
      \)"
  in do
    withConnIO $ \conn ->
      traverse_ (execute_ conn)
        [ enableForeignKeys
        , qGameState
        , qChatLines
        , qUsers
        ]
    _ <- insertGameState (NewDbGameState (newGame (PlayerId "Player1")))
    pure ()

insert ::
  ( DbConstraints e r m
  , ToRow a
  )
  => Query
  -> a
  -> m Integer
insert q a =
  withConnIO $ \conn -> do
    execute conn q a
    fromIntegral <$> lastInsertRowId conn

query_' :: (DbConstraints e r m, FromRow a) => Query -> m [a]
query_' q = withConnIO (\c -> query_ c q)

query' :: (DbConstraints e r m, FromRow a, ToRow i) => Query -> i -> m [a]
query' q i = withConnIO (\c -> query c q i)

execute' :: (DbConstraints e r m, ToRow i) => Query -> i -> m ()
execute' q i = withConnIO (\c -> execute c q i)

withConnIO ::
  DbConstraints e r m
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
