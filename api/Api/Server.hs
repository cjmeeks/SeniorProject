{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Server
     ( server
     ) where

import Servant ((:<|>)((:<|>)), Server, serveDirectoryFileServer)
import Api.Types (ApiWithAssets)
import Api.User.Handler (userHandler)

server :: Server ApiWithAssets
server = userHandler' :<|> serveStatic'
    where
        userHandler' = userHandler
        serveStatic' = serveDirectoryFileServer "./frontend/dist/static"