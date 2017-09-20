{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Server
     ( server
     ) where

import Servant ((:<|>)((:<|>)), Server, serveDirectoryFileServer)
import Api.Types (ApiWithAssets)
import Api.App.Handler (rollDice)

server :: Server ApiWithAssets
server = rollDice' :<|> serveStatic'
    where
        rollDice' = rollDice
        serveStatic' = serveDirectoryFileServer "./frontend/dist/static"