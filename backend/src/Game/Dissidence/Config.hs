{-# LANGUAGE DeriveGeneric #-}
module Game.Dissidence.Config where

import Data.Text
import Data.Time          (NominalDiffTime)
import Dhall

data DbConfig = DbConfig
  { connStr  :: Text
  , poolSize :: Natural
  , timeout  :: Natural
  } deriving (Generic, Show)

instance Interpret DbConfig

data Config = Config { db :: DbConfig } deriving (Generic, Show)

instance Interpret Config

load :: Text -> IO Config
load = input auto
