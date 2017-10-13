{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Server
     ( server
     ) where

import Servant ((:<|>)((:<|>)), Server, serveDirectoryFileServer)
import Api.Types (ApiWithAssets)
import Api.User.Handler (userHandler)
import Api.Workout.Handler (addWorkout)

server :: Server ApiWithAssets
server = (userHandler' :<|> addWorkout') :<|> serveStatic'
    where
        userHandler' = userHandler
        serveStatic' = serveDirectoryFileServer "./frontend/dist/static"
        addWorkout' = addWorkout