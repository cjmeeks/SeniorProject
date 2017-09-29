{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Api.Config
    ( DbConfig(..)
    , defaultConfig
    ) where

import Servant ((:<|>), (:>), Post, JSON, ReqBody, Raw)
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Data.UUID (UUID, toText)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple (ConnectInfo(..))

data DbConfig = DbConfig
        { postConfig :: ConnectInfo
        } deriving (Show)

defaultConfig = DbConfig (ConnectInfo  "localhost" 5432 "cjmeeks" "cjmeeksdb" "senior_project")