{-# LANGUAGE OverloadedStrings #-}

module Api.User.Handler
    ( userHandler
    ) where

import Servant (Handler)        
import Control.Monad.IO.Class (liftIO)
import System.Random (getStdRandom, randomR)
import Api.Types (User(..))
import Api.Config (defaultConfig, DbConfig(..))

userHandler :: String -> Handler Int
userHandler dice = undefined
    
getUser ::  DbConfig -> Int -> User
getUser config userid =
    undefined
