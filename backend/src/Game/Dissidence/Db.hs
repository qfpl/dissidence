{-# LANGUAGE ConstraintKinds, DataKinds, DeriveGeneric, FlexibleContexts, NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TemplateHaskell, TypeApplications   #-}

module Game.Dissidence.Db
  ( initDb
  , DbConstraints
--  , insertUser
--  , upsertGameState
--  , selectUser
 -- , selectGameState
  , runDb
  ) where

import Control.Lens

import Control.Monad                      ((<=<))
import Control.Monad.Error.Lens           (throwing)
import Control.Monad.Except               (MonadError)
import Control.Monad.IO.Class             (MonadIO, liftIO)
import Control.Monad.Reader               (MonadReader, ask)
import Data.Foldable                      (traverse_)
import Data.Int                           (Int64)
import Data.Text                          (Text)
import Database.SQLite.Simple             (Connection, Query, ToRow, execute, execute_, lastInsertRowId)
import Database.SQLite.SimpleErrors       (runDBAction)
import Database.SQLite.SimpleErrors.Types (SQLiteResponse)
import GHC.Generics                       (Generic)
import Servant.Elm                        (defaultOptions, deriveBoth)

type Posix = Integer

newtype GameId = GameId { unGameId :: Integer } deriving (Show, Generic)

data ChatLine = ChatLine
  { chatLineTime     :: Posix
  , chatLineGameId   :: Maybe GameId
  , chatLineUsername :: Text
  , chatLineText     :: Text
  } deriving (Show, Generic)

data NewChatLine = NewChatLine
  { newChatLineUsername :: Text
  , newChatLineText     :: Text
  } deriving (Show, Generic)

concat <$> mapM
  (deriveBoth defaultOptions)
  [''GameId, ''ChatLine, ''NewChatLine]

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
selectChatLines = _

initDb ::
  DbConstraints e r m
  => m ()
initDb =
  let
    enableForeignKeys = "PRAGMA foreign_keys = ON;"
    qGameState = "CREATE TABLE IF NOT EXISTS game_state\
      \( id INTEGER PRIMARY KEY\
      \, timestamp TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP\
      \, data TEXT NOT NULL\
      \)"
    qChatLines = "CREATE TABLE IF NOT EXISTS chat_line\
      \( id INTEGER PRIMARY KEY\
      \, timestamp TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP\
      \, game_id INTEGER REFERENCES game_state(id)\
      \, text TEXT NOT NULL\
      \)"
    qUsers = "CREATE TABLE IF NOT EXISTS user\
      \( id INTEGER PRIMARY KEY\
      \, username TEXT NOT NULL UNIQUE\
      \)"
  in
    withConnIO $ \conn ->
      traverse_ (execute_ conn) [
        enableForeignKeys
      , qGameState
      , qChatLines
      , qUsers
      ]

_insert ::
  ( DbConstraints e r m
  , ToRow a
  )
  => Query
  -> a
  -> m Int64
_insert q a =
  withConnIO $ \conn -> do
    execute conn q a
    lastInsertRowId conn

_withConn ::
  DbConstraints e r m
  => (Connection -> m a)
  -> m a
_withConn f =
  (f . view connection) =<< ask

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
