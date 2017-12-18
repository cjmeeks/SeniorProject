{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Api.Set.Handler
    ( addSet
    , deleteSet
    , getSetId
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

getSetId :: Handler Int
getSetId  = do
    conn <- liftIO $ connect (postConfig defaultConfig)
    numRows <- liftIO $ (query_ conn eQuery :: IO [Set])
    _ <- liftIO $ close conn
    case length numRows of
        0 -> return 1
        _ -> return $ (set_id (numRows !! 0) + 1)

eQuery :: Query
eQuery =
    [sql|
        SELECT * FROM workout.sets ORDER BY set_id DESC LIMIT 1
    |]

addSet :: Int -> Set -> Handler Int
addSet eid set = do
    let weight = (set_weight set)
        reps = (set_reps set)
        exercise_id = (set_exercise_id set)
        lift = (set_lift_id set)
    conn <- liftIO $ connect (postConfig defaultConfig)
    numRows <- liftIO $ execute conn addSQuery [weight, reps, eid, lift]
    _ <- liftIO $ close conn
    case numRows of
        0 -> throwError $ err400 {errBody = "(workout add) No workout added"}
        _ -> return 1

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