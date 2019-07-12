{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts, NamedFieldPuns, OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, TypeApplications                          #-}

module Game.Dissidence.Db
  ( initDb
--  , insertUser
--  , upsertGameState
--  , selectUser
 -- , selectGameState
  , runDb
  ) where

import Control.Lens

import           Control.Monad                      ((<=<))
import           Control.Monad.Error.Lens           (throwing)
import           Control.Monad.Except               (MonadError)
import           Control.Monad.IO.Class             (MonadIO, liftIO)
import           Control.Monad.Reader               (MonadReader, ask)
import           Data.Foldable                      (traverse_)
import           Data.Int                           (Int64)
import qualified Data.Text                          as T
import           Database.SQLite.Simple             (Connection, Query (Query), ToRow, execute, execute_,
                                                     lastInsertRowId)
import           Database.SQLite.SimpleErrors       (runDBAction)
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)


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

initDb ::
  DbConstraints e r m
  => m ()
initDb =
  let
    enableForeignKeys = "PRAGMA foreign_keys = ON;"
    toQ = Query . T.intercalate "\n"
    qGameState = toQ [
        "CREATE TABLE IF NOT EXISTS game_state"
      , "( id INTEGER PRIMARY KEY"
      , ", timestamp TEXT NOT NULL DEFAULT CURRENT_TIMESTAMP"
      , ", data TEXT NOT NULL"
      , ")"
      ]
  in
    withConnIO $ \conn ->
      traverse_ (execute_ conn) [
        enableForeignKeys
      , qGameState
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
