{-# LANGUAGE DeriveGeneric #-}
module Game.Dissidence.Config where

import Data.Text
import Dhall

data Config = Config { port :: Natural } deriving (Generic, Show)

instance Interpret Config

load :: Text -> IO Config
load = input auto
