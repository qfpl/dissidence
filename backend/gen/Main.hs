{-# LANGUAGE OverloadedStrings #-}
module Main where

import Game.Dissidence
import Servant.Elm

myElmOpts :: ElmOptions
myElmOpts = defElmOptions { urlPrefix = Static "http://localhost:8001/" }

main :: IO ()
main =
  generateElmModuleWith
    myElmOpts
    [ "Generated"
    , "Api"
    ]
    defElmImports
    "../frontend/api/"
    [ DefineElm (Proxy :: Proxy ChatLine)
    , DefineElm (Proxy :: Proxy NewChatLine)
    ]
    (Proxy :: Proxy Api)
