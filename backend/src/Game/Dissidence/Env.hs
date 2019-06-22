{-# LANGUAGE DataKinds, TypeApplications, DeriveGeneric #-}
module Game.Dissidence.Env where

import Control.Lens
import Data.Generics.Product (field)
import Data.Text.Encoding    (encodeUtf8)
import GHC.Generics          (Generic)
import Hasql.Pool            (Pool, acquire)

import Game.Dissidence.Config

data Env = Env { dbPool :: Pool } deriving (Show, Generic)

configToEnv :: Config -> IO Env
configToEnv c = do
  let dbC = c ^. field @"db"
  p <- acquire
    ( dbC ^. field @"poolSize" . to fromIntegral
    , dbC ^. field @"timeout" . to fromIntegral
    , dbC ^. field @"connStr" . to encodeUtf8
    )
  pure $ Env p
