{-# LANGUAGE ConstraintKinds, DataKinds, DeriveGeneric, FlexibleContexts, LambdaCase, NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, TemplateHaskell, TypeApplications               #-}
module Game.Dissidence.Db
  ( initDb
  , DbConstraints
  , selectChatLines
  , insertChatLine
  , selectGameState
  , insertGameState
  , updateGameState
  , insertGameStateEvent
  , listGameStateEvents
  , listJoinableGames
  , insertPlayer
  , checkLogin
  , findPlayer
  , runDb
  , ChatLine
  , NewChatLine(..)
  , DbGameState
  , JoinableGame(..)
  , NewDbGameState(..)
  , NewDbGameStateEvent(..)
  , DbPlayer
  , AsSQLiteResponse(..)
  , AsDbError(..)
  , AsDbLogicError(..)
  , HasConnection(..)
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
import           Data.Maybe                         (fromMaybe, isNothing, mapMaybe)
import qualified Data.Set                           as Set
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
import           Servant                            (FromHttpApiData (..))
import           Servant.Elm                        (deriveBoth)

import Game.Dissidence.AesonOptions       (ourAesonOptions)
import Game.Dissidence.GameState          (PlayerId (PlayerId, unPlayerId), toGameStateType)
import Game.Dissidence.GameState.Internal (GameState (WaitingForPlayers), GameStateInputEvent,
                                           GameStateInternalEvent, GameStateOutputEvent)

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

instance FromHttpApiData GameId where
  parseUrlPiece = fmap GameId . parseUrlPiece


data ChatLine = ChatLine
  { chatLineTime     :: Posix
  , chatLineGameId   :: Maybe GameId
  , chatLinePlayerId :: PlayerId
  , chatLineText     :: Text
  } deriving (Show, Generic)

instance FromRow ChatLine where
  fromRow = ChatLine <$> field <*> field <*> (PlayerId <$> field) <*> field

data NewChatLine = NewChatLine
  { newChatGameId       :: Maybe GameId
  , newChatLinePlayerId :: PlayerId
  , newChatLineText     :: Text
  } deriving (Show, Generic)

instance ToRow NewChatLine where
  toRow (NewChatLine gId pId t) = [toField gId, toField (unPlayerId pId), toField t]

data DbGameState = DbGameState
  { dbGameStateId :: GameId
  , dbGameState   :: GameState
  } deriving (Show, Generic)

instance FromRow DbGameState where
  fromRow = DbGameState <$> (GameId <$> field) <*> aesonField

newtype NewDbGameState = NewDbGameState { unNewDbGameState :: GameState } deriving (Show, Generic)

instance ToRow NewDbGameState where
  toRow (NewDbGameState gs) = [toField (toGameStateType gs), toAesonField gs]

data JoinableGame = JoinableGame
  { joinableGameId :: GameId
  , playerCount    :: Int
  }

data DbPlayer = DbPlayer
  { dbPlayerId       :: PlayerId
  , dbPlayerPassword :: Text
  } deriving (Show, Generic)

instance FromRow DbPlayer where
  fromRow = DbPlayer <$> (PlayerId <$> field) <*> field

instance ToRow DbPlayer where
  toRow (DbPlayer pId p) = [toField (unPlayerId pId), toField p]

data NewDbGameStateEvent = NewDbGameStateEvent
  { newDbGameStateEventGameId   :: GameId
  , newDbGameStatePlayerId      :: PlayerId
  , newDbGameStateEventInput    :: GameStateInputEvent
  , newDbGameStateEventInternal :: Maybe GameStateInternalEvent
  , newDbGameStateEventOutput   :: Maybe GameStateOutputEvent
  } deriving (Show, Generic)

instance ToRow NewDbGameStateEvent where
  toRow (NewDbGameStateEvent gId playerId input internal output) =
    [toField gId, toField (unPlayerId playerId), toAesonField input, toAesonField internal, toAesonField output]

data DbGameStateEvent = DbGameStateEvent
  { dbGameStateEventGameId   :: GameId
  , dbGameStatePlayerId      :: PlayerId
  , dbGameStateEventTime     :: Posix
  , dbGameStateEventInput    :: GameStateInputEvent
  , dbGameStateEventInternal :: Maybe GameStateInternalEvent
  , dbGameStateEventOutput   :: Maybe GameStateOutputEvent
  } deriving (Show, Generic)

instance FromRow DbGameStateEvent where
  fromRow = DbGameStateEvent <$> field <*> (PlayerId <$> field) <*> field <*> aesonField <*> aesonField <*> aesonField

concat <$> mapM
  (deriveBoth ourAesonOptions)
  [''GameId, ''DbGameState, ''NewDbGameState, ''JoinableGame, ''ChatLine, ''NewChatLine, ''DbPlayer]

class HasConnection s where
  connection :: Lens' s Connection

instance HasConnection Connection where
  connection = lens id (const id)

data DbLogicError = PlayerAlreadyExists PlayerId | GameDoesntExist GameId deriving (Eq, Show, Generic)
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

selectChatLines :: SqLiteConstraints e r m => Maybe Posix -> Maybe GameId -> m [ChatLine]
selectChatLines eMay gIdMay = query'
  "SELECT epoch, game_state_id, player_id, text FROM chat_line WHERE epoch > ? AND game_state_id IS ?"
  (fromMaybe 0 eMay,gIdMay)

insertChatLine :: DbConstraints e r m => NewChatLine -> m ()
insertChatLine = execute' "INSERT INTO chat_line (game_state_id,player_id, text) VALUES (?,?,?)"

selectGameState :: DbConstraints e r m => GameId -> m DbGameState
selectGameState gId = do
  gs <- query' "SELECT id, data FROM game_state WHERE id = ?" (Only gId)
  maybe (throwing _GameDoesntExist gId) pure (gs^?_head)

listJoinableGames :: DbConstraints e r m => m [JoinableGame]
listJoinableGames = do
  dbGameSs <- query_' "SELECT id, data FROM game_state WHERE state_type = 'waiting_for_players'"
  pure $ mapMaybe countWaiting dbGameSs
  where
    countWaiting (DbGameState gId (WaitingForPlayers _ ps)) = Just (JoinableGame gId (Set.size ps + 1))
    countWaiting _                                          = Nothing

updateGameState :: DbConstraints e r m => DbGameState -> m ()
updateGameState (DbGameState gId d) = execute'
  "UPDATE game_state SET data = ?, epoch = strftime('%s', CURRENT_TIMESTAMP), state_type = ? WHERE id = ?"
  (toAesonField d, toGameStateType d, gId)

insertGameState :: DbConstraints e r m => NewDbGameState -> m GameId
insertGameState = fmap GameId . insert "INSERT INTO game_state (state_type, data) VALUES (?,?)"

insertGameStateEvent :: DbConstraints e r m => NewDbGameStateEvent -> m ()
insertGameStateEvent ne = do
  _ <- selectGameState (newDbGameStateEventGameId ne)
  execute' "INSERT INTO game_state_event (game_state_id, player_id, input, internal, output) VALUES (?,?,?,?,?)" ne

listGameStateEvents :: DbConstraints e r m => GameId -> Maybe Posix -> m [DbGameStateEvent]
listGameStateEvents gId eMay = query'
  "SELECT game_state_id, player_id, epoch, input, internal, output FROM game_state_event WHERE game_state_id = ? and epoch > ?"
  (gId, fromMaybe 0 eMay)

findPlayer :: DbConstraints e r m => PlayerId -> m (Maybe DbPlayer)
findPlayer = fmap (^?_head) . query' "SELECT id, password FROM player WHERE id = ?" . Only . unPlayerId

insertPlayer :: DbConstraints e r m => DbPlayer -> m ()
insertPlayer (DbPlayer pId p) = do
  pMay <- findPlayer pId
  unless (isNothing pMay) $ throwing _PlayerAlreadyExists pId
  pwBs <- liftIO $ makePassword (encodeUtf8 p) 17
  execute' "INSERT INTO player (id, password) VALUES (?,?)" (unPlayerId pId,(decodeUtf8 pwBs))

checkLogin :: DbConstraints e r m => DbPlayer -> m Bool
checkLogin (DbPlayer pId p) = do
  pMay <- findPlayer pId
  pure $ maybe False (verifyPassword (encodeUtf8 p) . (^.GP.field @"dbPlayerPassword".to encodeUtf8)) pMay

initDb ::
  SqLiteConstraints e r m
  => m ()
initDb =
  let
    enableForeignKeys = "PRAGMA foreign_keys = ON;"
    qGameState = "CREATE TABLE IF NOT EXISTS game_state\
      \( id INTEGER PRIMARY KEY\
      \, epoch INTEGER NOT NULL DEFAULT (strftime('%s',CURRENT_TIMESTAMP))\
      \, state_type TEXT NOT NULL\
      \, data TEXT NOT NULL\
      \)"
    qGameStateEvent = "CREATE TABLE IF NOT EXISTS game_state_event\
      \( game_state_id INTEGER REFERENCES game_state(id)\
      \, player_id TEXT NOT NULL references player(id)\
      \, epoch INTEGER NOT NULL DEFAULT (strftime('%s',CURRENT_TIMESTAMP))\
      \, input TEXT NOT NULL\
      \, internal TEXT NOT NULL\
      \, output TEXT NOT NULL\
      \)"
    qChatLines = "CREATE TABLE IF NOT EXISTS chat_line\
      \( id INTEGER PRIMARY KEY\
      \, epoch INTEGER NOT NULL DEFAULT (strftime('%s',CURRENT_TIMESTAMP))\
      \, game_state_id INTEGER REFERENCES game_state(id)\
      \, player_id TEXT REFERENCES player(id)\
      \, text TEXT NOT NULL\
      \)"
    qPlayer = "CREATE TABLE IF NOT EXISTS player\
      \( id TEXT NOT NULL PRIMARY KEY \
      \, password TEXT NOT NULL\
      \)"
  in
    withConnIO $ \conn ->
      traverse_ (execute_ conn)
        [ enableForeignKeys
        , qGameState
        , qGameStateEvent
        , qChatLines
        , qPlayer
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

query' :: (SqLiteConstraints e r m, FromRow a, ToRow i) => Query -> i -> m [a]
query' q i = withConnIO (\c -> query c q i)

query_' :: (SqLiteConstraints e r m, FromRow a) => Query -> m [a]
query_' q = withConnIO (\c -> query_ c q)

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
