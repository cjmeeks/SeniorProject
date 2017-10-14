{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Api.Run.Handler
    ( addRun
    , deleteRun
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

addRun :: Maybe Float -> Maybe Double -> Maybe Float -> Maybe Float -> Maybe Int -> Handler Text
addRun dis time mileAvg speedAvg wkid = do
    dis' <- maybe (throwError $ err400 {errBody = "no distance"}) (return) dis
    time' <- maybe (throwError $ err400 {errBody = "no time"}) (return) time
    mileAvg' <- maybe (throwError $ err400 {errBody = "no mile avg"}) (return) mileAvg
    speedAvg' <- maybe (throwError $ err400 {errBody = "no speed"}) (return) speedAvg
    wkid' <- maybe (throwError $ err400 {errBody = "no workout id"}) (return) wkid
    conn <- liftIO $ connect (postConfig defaultConfig)
    numRows <- liftIO $ execute conn addRQuery (dis', time', mileAvg', speedAvg', wkid')
    _ <- liftIO $ close conn
    case numRows of
        0 -> throwError $ err400 {errBody = "(run add) No run added."}
        _ -> return "Success"

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