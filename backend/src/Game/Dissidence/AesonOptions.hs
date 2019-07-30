module Game.Dissidence.AesonOptions (ourAesonOptions, unwrapUnaryRecords) where

import Data.Aeson.TH

ourAesonOptions :: Options
ourAesonOptions = defaultOptions
  { unwrapUnaryRecords = True
  , allNullaryToStringTag = True
  , sumEncoding = TwoElemArray -- NOTE: jsonHelpers.decodeSumTaggedObject dies on a nullary constructor when there are
                               -- non-nullary constructors in the type (aeson doesn't send the contents key)
  }
