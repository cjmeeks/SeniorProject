{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Api.User.Handler
    ( userHandler
    ) where

import Servant (Handler, err500, errBody)        
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error.Class(throwError)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.List (length)
import System.Random (getStdRandom, randomR)
import Database.PostgreSQL.Simple (Connection, Query, close, connect, execute, query)
import Database.PostgreSQL.Simple.SqlQQ
import Api.Types (User(..), Workout(..), Run(..), Set(..), Exercise(..), Lift(..))
import Api.Config (defaultConfig, DbConfig(..))

userHandler :: Maybe Int -> Handler User
userHandler userid = do
    let userid' = fromMaybe 2 userid
    conn <- liftIO $ connect (postConfig defaultConfig)
    users <- liftIO $ getUser conn userid'
    let user = if length users > 0 then (Just (return $ users !! 0)) else Nothing
    newUser <- fromMaybe (throwError err500) user
    workouts <- liftIO $ getWorkouts conn (user_id newUser)
    newWorkouts <- mapM (getRuns conn) workouts
    finalUser <- return $ newUser { user_workouts = newWorkouts }
    _ <- liftIO $ close conn
    return finalUser



    
getUser ::  Connection -> Int -> IO [User]
getUser conn userid = do
    query conn queryUser [userid] :: IO [User]

getWorkouts :: Connection -> Int -> IO [Workout]
getWorkouts conn userid = do
    query conn queryWorkouts [userid] :: IO [Workout]

getRuns :: Connection -> Workout -> Handler Workout
getRuns conn wk = do
    runs <- liftIO $ query conn queryRuns [workout_id wk]
    return $ wk { workout_runs = runs }

queryUser :: Query
queryUser =
    [sql|
        SELECT * FROM workout.users WHERE workout.users.user_id = ?
    |]

queryWorkouts :: Query
queryWorkouts =
    [sql|
        SELECT * FROM workout.workouts WHERE user_id = ?
    |]

queryRuns :: Query
queryRuns =
    [sql|
        SELECT * FROM workout.run WHERE workout_id = ?
    |]
