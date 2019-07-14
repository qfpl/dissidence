module Game.Dissidence.AesonOptions where

import Data.Aeson (Options, defaultOptions, unwrapUnaryRecords)

ourAesonOptions :: Options
ourAesonOptions = defaultOptions { unwrapUnaryRecords = True }
