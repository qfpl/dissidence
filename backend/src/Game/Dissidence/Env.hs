{-# LANGUAGE DataKinds, DeriveGeneric, NamedFieldPuns, RankNTypes, TemplateHaskell, TypeApplications #-}
module Game.Dissidence.Env where

import Control.Lens

import           Crypto.JOSE.JWK        (KeyMaterial (OctKeyMaterial), OctKeyParameters (OctKeyParameters),
                                         fromKeyMaterial)
import           Crypto.JOSE.Types      (Base64Octets (Base64Octets))
import qualified Data.ByteString.Char8  as C8
import           Data.Generics.Product  (field)
import           Database.SQLite.Simple (Connection, withConnection)
import           GHC.Generics           (Generic)
import           Servant.Auth.Server

import Game.Dissidence.Config
import Game.Dissidence.Db     (HasConnection (..))

data Env = Env
  { _envDbPath         :: String
  , _envCookieSettings :: CookieSettings
  , _envJwtSettings    :: JWTSettings
  } deriving (Generic)
makeClassy 'Env

data EnvWConnection = EnvWConnection
  { _envDbConnection :: Connection
  , _envEnv          :: Env
  }
makeLenses 'EnvWConnection

instance HasConnection EnvWConnection where
  connection = envDbConnection

instance HasEnv EnvWConnection where
  env = envEnv . env

configToEnv :: Config -> IO Env
configToEnv c = do
  pure $ Env
    (c ^. field @"dbPath")
    defaultCookieSettings
    (defaultJWTSettings
      . fromKeyMaterial
      . OctKeyMaterial
      . OctKeyParameters
      . Base64Octets
      . C8.pack
      $ c ^. field @"jwtKey")

withEnvDbConnection :: Env -> (EnvWConnection -> IO a) -> IO a
withEnvDbConnection e f = withConnection (e^.envDbPath) (\c -> f (EnvWConnection c e))
