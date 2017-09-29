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
import Api.Types (User(..))
import Api.Config (defaultConfig, DbConfig(..))

userHandler :: Maybe Int -> Handler User
userHandler userid = do
    let userid' = fromMaybe 2 userid
    users <- liftIO $ getUser defaultConfig userid'
    if length users > 0 then return $ users !! 0 else throwError err500 
    
    
getUser ::  DbConfig -> Int -> IO [User]
getUser config userid = do
    conn <- liftIO $ connect (postConfig config)
    query conn queryUser [userid] :: IO [User]


queryUser :: Query
queryUser =
    [sql|
        SELECT * FROM workout.users WHERE workout.users.user_id = ?
    |]


