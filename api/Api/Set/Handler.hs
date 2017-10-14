{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Api.Set.Handler
    ( addSet
    , deleteSet
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

addSet :: Maybe Int -> Maybe Int -> Maybe Int -> Maybe Int -> Handler Text
addSet weight reps exercise_id lift_id = do
    weight' <- maybe (throwError $ err400 {errBody = "no weighttance"}) (return) weight
    reps' <- maybe (throwError $ err400 {errBody = "no mile avg"}) (return) reps
    exercise_id' <- maybe (throwError $ err400 {errBody = "no speed"}) (return) exercise_id
    lift_id' <- maybe (throwError $ err400 {errBody = "no workout id"}) (return) lift_id
    conn <- liftIO $ connect (postConfig defaultConfig)
    numRows <- liftIO $ execute conn addSQuery (weight', reps', exercise_id', lift_id')
    _ <- liftIO $ close conn
    case numRows of
        0 -> throwError $ err400 {errBody = "(set add) No set added."}
        _ -> return "Success"

addSQuery :: Query
addSQuery =
    [sql|
        INSERT INTO workout.sets (weight, reps, exercise_id, lift_id) VALUES (?, ?, ?, ?)
    |]

deleteSet :: Maybe Int -> Maybe Int -> Handler Text
deleteSet sid userid= do
    sid' <- maybe (throwError $ err400 {errBody = "no sid"}) (return) sid
    userid' <- maybe (throwError $ err400 {errBody = "no userId"}) (return) userid
    conn <- liftIO $ connect (postConfig defaultConfig)
    numRows <- liftIO $ execute conn deleteSQuery [sid']
    _ <- liftIO $ close conn
    case numRows of
        0 -> throwError $ err400 {errBody = "(set delete) No set deleted."}
        _ -> return "Success"



deleteSQuery :: Query
deleteSQuery =
    [sql|
        DELETE FROM workout.sets WHERE set_id = ?
    |]