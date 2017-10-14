{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Api.Workout.Handler
    ( addWorkout
    , deleteWk
    ) where

import Servant (Handler, err500, errBody, err400)        
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error.Class(throwError)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.List (length)
import Data.Time (UTCTime)
import System.Random (getStdRandom, randomR)
import Database.PostgreSQL.Simple (Connection, Query, close, connect, execute, query)
import Database.PostgreSQL.Simple.SqlQQ
import Api.Types (ElmUTCTime, User(..), Workout(..), Run(..), Set(..), Exercise(..), Lift(..))
import Api.Config (defaultConfig, DbConfig(..))
import System.IO.Unsafe (unsafePerformIO)
import Data.String.Conversions (cs)
import Data.Text (Text)

addWorkout :: Maybe Text -> Maybe Double -> Maybe Text -> Maybe Int -> Handler Text
addWorkout date total_time wType userid = do
    date' <- maybe (throwError $ err400 {errBody = "no date"}) (\x -> return $ (cs x :: String)) date
    total_time' <- maybe (throwError $ err400 {errBody = "no total time"}) (return) total_time
    wType' <- maybe (throwError $ err400 {errBody = "no workout type"}) (return) wType
    userid' <- maybe (throwError $ err400 {errBody = "no user id"}) (return) userid
    conn <- liftIO $ connect (postConfig defaultConfig)
    numRows <- liftIO $ execute conn addWQuery (((read date')::UTCTime), total_time', wType', userid')
    _ <- liftIO $ close conn
    case numRows of
        0 -> throwError $ err400 {errBody = "(workout add) No workout added."}
        _ -> return "asdf"

addWQuery :: Query
addWQuery =
    [sql|
        INSERT INTO workout.workouts (date, total_time, workout_type, user_id) VALUES (?, ?, ?, ?)
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