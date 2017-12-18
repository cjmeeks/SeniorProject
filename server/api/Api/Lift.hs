{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Api.Lift
    ( getLifts
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

getLifts :: Handler [Lift]
getLifts = do
    conn <- liftIO $ connect (postConfig defaultConfig)
    lifts <- liftIO $ query_ conn selectAllLiftsQuery
    _ <- liftIO $ close conn
    case length lifts of
        0 -> throwError $ err400 {errBody = "noLifts."}
        _ -> return lifts

selectAllLiftsQuery :: Query
selectAllLiftsQuery =
    [sql|
        SELECT * FROM  workout.lifts
    |]

-- getThem :: Maybe Int -> Maybe Int -> Handler Text
-- getThem sid userid= do
--     sid' <- maybe (throwError $ err400 {errBody = "no sid"}) (return) sid
--     userid' <- maybe (throwError $ err400 {errBody = "no userId"}) (return) userid
--     conn <- liftIO $ connect (postConfig defaultConfig)
--     numRows <- liftIO $ execute conn deleteSQuery [sid']
--     _ <- liftIO $ close conn
--     case numRows of
--         0 -> throwError $ err400 {errBody = "(set delete) No set deleted."}
--         _ -> return "Success"