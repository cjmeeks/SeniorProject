{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Server
     ( server
     ) where

import Servant ((:<|>)((:<|>)), Server, serveDirectoryFileServer)
import Api.Types (ApiWithAssets)
import Api.User.Handler (userHandler)
import Api.Workout.Handler (addWorkout, deleteWk)
import Api.Exercise.Handler (addExercise, deleteExercise)
import Api.Run.Handler (addRun, deleteRun)
import Api.Set.Handler (addSet, deleteSet)

server :: Server ApiWithAssets
server = (userHandler' :<|> (addWorkout' :<|> addExercise' :<|> addRun' :<|> addSet') :<|> (deleteWk' :<|> deleteExercise' :<|> deleteRun' :<|> deleteSet')) :<|> serveStatic'
    where
        userHandler' = userHandler
        serveStatic' = serveDirectoryFileServer "./frontend/dist/static"
        addWorkout' = addWorkout
        addExercise' = addExercise
        addRun' = addRun
        addSet' = addSet
        deleteWk' = deleteWk
        deleteExercise' = deleteExercise
        deleteRun' = deleteRun
        deleteSet' = deleteSet