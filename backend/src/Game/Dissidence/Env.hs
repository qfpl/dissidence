{-# LANGUAGE DataKinds, DeriveGeneric, TypeApplications #-}
module Game.Dissidence.Env where

import GHC.Generics (Generic)

import Game.Dissidence.Config

data Env = Env { } deriving (Show, Generic)

configToEnv :: Config -> IO Env
configToEnv _ = do
  pure $ Env
