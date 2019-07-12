{-# LANGUAGE DataKinds, DeriveGeneric, NamedFieldPuns, RankNTypes, TypeApplications #-}
module Game.Dissidence.Env where

import Control.Lens

import Data.Generics.Product  (field)
import Database.SQLite.Simple (Connection, withConnection)
import GHC.Generics           (Generic)

import Game.Dissidence.Config

data Env = Env
  { dbPath :: String
  } deriving (Show, Generic)

configToEnv :: Config -> IO Env
configToEnv c = do
  pure $ Env (c ^. field @"dbPath")

withEnvDbConnection :: Env -> (Connection -> IO a) -> IO a
withEnvDbConnection = withConnection . (^. field @"dbPath")
