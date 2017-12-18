{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Api.Exercise.Handler
    ( addExercise
    , deleteExercise
    , getExerciseId
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
import Api.Set.Handler (addSet, getSetId)

getExerciseId :: Handler Int
getExerciseId = do
    conn <- liftIO $ connect (postConfig defaultConfig)
    numRows <- liftIO $ (query_ conn eQuery :: IO [Exercise])
    _ <- liftIO $ close conn
    case length numRows of
        0 -> return 1
        _ -> return $ (exercise_id (numRows !! 0) + 1)

eQuery :: Query
eQuery =
    [sql|
        SELECT * FROM workout.exercise ORDER BY exercise_id DESC LIMIT 1
    |]

addExercise :: Int -> Exercise -> Handler Int
addExercise wid ex = do
    let time = (exercise_time ex)
        workoutid = (exercise_workout_id ex)
    conn <- liftIO $ connect (postConfig defaultConfig)
    numRows <- liftIO $ execute conn addEQuery [time, wid]
    eid <- getExerciseId
    handleSets<- mapM (addSet (eid - 1)) (exercise_sets ex)
    _ <- liftIO $ close conn
    case numRows of
        0 -> throwError $ err400 {errBody = "(workout add) No workout added"}
        _ -> return 1

addEQuery :: Query
addEQuery =
    [sql|
        INSERT INTO workout.exercise (time, workout_id) VALUES (?, ?)
    |]

deleteExercise :: Maybe Int -> Maybe Int -> Handler Text
deleteExercise eid userid = do
    eid' <- maybe (throwError $ err400 {errBody = "no eid"}) (return) eid
    userid' <- maybe (throwError $ err400 {errBody = "no userId"}) (return) userid
    conn <- liftIO $ connect (postConfig defaultConfig)
    numRows <- liftIO $ execute conn deleteEQuery [eid']
    _ <- liftIO $ close conn
    case numRows of
        0 -> throwError $ err400 {errBody = "(workout add) No eid deleted."}
        _ -> return "Success"



deleteEQuery :: Query
deleteEQuery =
    [sql|
        DELETE FROM workout.exercise WHERE exercise_id = ?
    |]