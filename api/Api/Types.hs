{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Api.Types
    ( Api
    , ApiWithAssets
    , User
    ) where

import Servant ((:<|>), (:>), Post, JSON, ReqBody, Raw)
import Api.App.Types (Dice)
import Elm (ElmType(..))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text (Text)

type Api
        = "api"
            :> ("rollDice" :> ReqBody '[JSON] Dice
                           :> Post '[JSON] Int
               )

type ApiWithAssets = (Api :<|> Raw)

data User = User 
        { user_id :: Int
        , user_first_name :: Text
        , user_last_name :: Text
        , user_username :: Text
        , user_weight :: Float
        , user_height :: Float
        , user_age ::Int
        } deriving (Show, Generic, ElmType, ToJSON, FromJSON)