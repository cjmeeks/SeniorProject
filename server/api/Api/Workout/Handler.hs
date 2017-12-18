{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Api.Workout.Handler
    ( addWorkout
    , deleteWk
    , getCurrentWorkoutId
    , addSavedWorkout
    ,deleteSavedWorkout
    ) where

import Servant (Handler, err500, errBody, err400)        
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error.Class(throwError)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.List (length)
import Data.Time (UTCTime)
import System.Random (getStdRandom, randomR)
import Database.PostgreSQL.Simple (Connection, Query, close, connect, execute, query, query_)
import Database.PostgreSQL.Simple.SqlQQ
import Api.Types (ElmUTCTime, User(..), Workout(..), Run(..), Set(..), Exercise(..), Lift(..))
import Api.Config (defaultConfig, DbConfig(..))
import System.IO.Unsafe (unsafePerformIO)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Api.User.Handler (userHandler)
import Api.Run.Handler (addRun)
import Api.Exercise.Handler (addExercise)

addWorkout :: Maybe Int -> Workout -> Handler User
addWorkout userid workout = do
    -- workout' <- maybe (throwError $ err400 {errBody = "no workout"}) (return) workout
    let date' = (workout_date workout)
        total_time' = (workout_total_time workout)
        wType' = (workout_name workout)
        userid' = userid
        exercises' = workout_exercises workout
        runs' = workout_runs workout
    conn <- liftIO $ connect (postConfig defaultConfig)
    numRows <- liftIO $ execute conn addWQuery ( date', total_time', wType', userid')
    _ <- liftIO $ close conn
    curWid <- getCurrentWorkoutId
    handleRuns <-  mapM (addRun (curWid - 1)) runs'
    handleExs <-  mapM (addExercise (curWid - 1)) exercises'
    case numRows of
        0 -> throwError $ err400 {errBody = "(workout add) No workout added."}
        _ -> userHandler userid

getCurrentWorkoutId :: Handler Int
getCurrentWorkoutId = do
    conn <- liftIO $ connect (postConfig defaultConfig)
    numRows <- liftIO $ (query_ conn wQuery :: IO [Workout])
    _ <- liftIO $ close conn
    case length numRows of
        0 -> return 1
        _ -> return $ (workout_id (numRows !! 0) + 1)

wQuery :: Query
wQuery =
    [sql|
        SELECT * FROM workout.workouts ORDER BY workout_id DESC LIMIT 1
    |]

addWQuery :: Query
addWQuery =
    [sql|
        INSERT INTO workout.workouts (date, total_time, workout_name, user_id) VALUES (?, ?, ?, ?)
    |]

deleteWk :: Maybe Int -> Maybe Int -> Handler Text
deleteWk wid userid= do
    wid' <- maybe (throwError $ err400 {errBody = "no rid"}) (return) wid
    userid' <- maybe (throwError $ err400 {errBody = "no userId"}) (return) userid
    conn <- liftIO $ connect (postConfig defaultConfig)
    numRows <- liftIO $ execute conn deleteWQuery [wid']
    _ <- liftIO $ close conn
    case numRows of
        0 -> throwError $ err400 {errBody = "(workout delete) No workout deleted."}
        _ -> return "Success"


deleteWQuery :: Query
deleteWQuery =
    [sql|
        DELETE FROM workout.workouts WHERE workout_id = ?
    |]

addSavedWorkout :: Maybe Int -> Maybe Int -> Handler Text
addSavedWorkout userid wid = do 
    wid' <- maybe (throwError $ err400 {errBody = "no rid"}) (return) wid
    userid' <- maybe (throwError $ err400 {errBody = "no userId"}) (return) userid
    conn <- liftIO $ connect (postConfig defaultConfig)
    -- prin <- return $ putStrLn "here"
    numRows <- liftIO $ execute conn addSavedQuery [userid', wid']
    _ <- liftIO $ close conn
    case numRows of
        0 -> throwError $ err400 {errBody = "(workout delete) No workout deleted."}
        _ -> return "Success"

addSavedQuery :: Query
addSavedQuery =
    [sql|
        INSERT INTO workout.saved_workouts (user_id, workout_id ) VALUES (?, ?)
    |]

deleteSavedWorkout :: Maybe Int  -> Handler Text
deleteSavedWorkout wid= do
    wid' <- maybe (throwError $ err400 {errBody = "no rid"}) (return) wid
    conn <- liftIO $ connect (postConfig defaultConfig)
    numRows <- liftIO $ execute conn deleteSWQuery [wid']
    _ <- liftIO $ close conn
    case numRows of
        0 -> throwError $ err400 {errBody = "(workout delete) No workout deleted."}
        _ -> return "Success"


deleteSWQuery :: Query
deleteSWQuery =
    [sql|
        DELETE FROM workout.saved_workouts WHERE workout_id = ?
    |]
