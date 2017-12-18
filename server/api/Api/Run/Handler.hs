{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Api.Run.Handler
    ( addRun
    , deleteRun
    , getRunId
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

getRunId ::  Handler Int
getRunId = do
    conn <- liftIO $ connect (postConfig defaultConfig)
    runs <- liftIO $ (query_ conn rQuery :: IO [Run])
    _ <- liftIO $ close conn
    case length runs of
        0 -> return 1
        _ -> return $ (run_id (runs !! 0) + 1)

rQuery :: Query
rQuery =
    [sql|
        SELECT * FROM workout.run ORDER BY run_id DESC LIMIT 1
    |]

addRun :: Int -> Run -> Handler Int
addRun wid run = do
    let distance = (run_distance run)
        thisNM' = run_workout_id run :: Int
        time = (run_time run)
        mile_avg = (run_mile_avg run)
        speed_avg = (run_speed_avg run)
    conn <- liftIO $ connect (postConfig defaultConfig)
    numRows <- liftIO $ execute conn addRQuery (distance, time, mile_avg, speed_avg, wid)
    _ <- liftIO $ close conn
    case numRows of
        0 -> throwError $ err400 {errBody = "(workout add) No workout added"}
        _ -> return 1

addRQuery :: Query
addRQuery =
    [sql|
        INSERT INTO workout.run (distance, time, mile_avg, speed_avg, workout_id) VALUES (?, ?, ?, ?, ?)
    |]

deleteRun :: Maybe Int -> Maybe Int -> Handler Text
deleteRun rid userid= do
    rid' <- maybe (throwError $ err400 {errBody = "no rid"}) (return) rid
    userid' <- maybe (throwError $ err400 {errBody = "no userId"}) (return) userid
    conn <- liftIO $ connect (postConfig defaultConfig)
    numRows <- liftIO $ execute conn deleteRQuery [rid']
    _ <- liftIO $ close conn
    case numRows of
        0 -> throwError $ err400 {errBody = "(run delete) No eid deleted."}
        _ -> return "Success"



deleteRQuery :: Query
deleteRQuery =
    [sql|
        DELETE FROM workout.run WHERE run_id = ?
    |]