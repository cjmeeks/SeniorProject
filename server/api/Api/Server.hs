{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Api.Server
     ( server
     ) where

import Servant ((:<|>)((:<|>)), Server, serveDirectoryFileServer)
import Api.Types (ApiWithAssets)
import Api.User.Handler (userHandler, loginHandler, singupHandler)
import Api.Workout.Handler (addWorkout, deleteWk, getCurrentWorkoutId, addSavedWorkout, deleteSavedWorkout)
import Api.Exercise.Handler (getExerciseId, deleteExercise)
import Api.Run.Handler (getRunId, deleteRun)
import Api.Set.Handler (getSetId, deleteSet)
import Api.Lift (getLifts)

server :: Server ApiWithAssets
server = ((userHandler':<|> loginHandler' :<|> singupHandler' :<|> liftHandler' :<|> (addWorkout' :<|> getExerciseId' :<|> getRunId' :<|> getSetId' :<|> getWorkoutId' :<|> addSavedWorkout') :<|> (deleteWk' :<|> deleteExercise' :<|> deleteRun' :<|> deleteSet' :<|> deleteSavedWorkout'))) :<|> serveStatic'
    where
        userHandler' = userHandler
        liftHandler' = getLifts
        serveStatic' = serveDirectoryFileServer "../client/dist"
        addWorkout' = addWorkout
        getWorkoutId' = getCurrentWorkoutId
        getExerciseId' = getExerciseId
        getRunId' = getRunId
        getSetId' = getSetId
        deleteWk' = deleteWk
        deleteExercise' = deleteExercise
        deleteRun' = deleteRun
        deleteSet' = deleteSet
        addSavedWorkout' = addSavedWorkout
        deleteSavedWorkout' =deleteSavedWorkout
        loginHandler' = loginHandler
        singupHandler' = singupHandler