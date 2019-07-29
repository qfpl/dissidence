module Game.Dissidence.AesonOptions (ourAesonOptions, unwrapUnaryRecords) where

import Data.Aeson (Options, allNullaryToStringTag, defaultOptions, unwrapUnaryRecords)

ourAesonOptions :: Options
ourAesonOptions = defaultOptions { unwrapUnaryRecords = True, allNullaryToStringTag = True }
