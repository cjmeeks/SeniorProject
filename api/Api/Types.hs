{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Api.Types
    ( Api
    , ApiWithAssets
    , UserInfo
    , Workout
    , Exercise
    , Run
    , User
    , Lift
    , ElmUTCTime(..)
    ) where

import Servant ((:<|>), (:>), Post, JSON, ReqBody, Raw)
import Api.App.Types (Dice)
import Api.Elm (ElmUTCTime(..))
import Elm (ElmType(..))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Data.UUID (UUID, toText)
import GHC.Generics (Generic)

type Api
        = "api"
            :> ("rollDice" :> ReqBody '[JSON] Dice
                           :> Post '[JSON] Int
               )

type ApiWithAssets = (Api :<|> Raw)


data UserInfo = UserInfo
    { user_id :: Int
    , user_first_name :: Text
    , user_last_name :: Text
    , user_username :: Text
    , user_weight :: Float
    , user_height :: Float
    , user_age ::Int
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

data User = User
    { userInfo :: UserInfo
    , workouts :: [Workout]
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

data Workout = Workout
    { workout_id :: Int
    , workout_date :: ElmUTCTime
    , workout_total_time :: Double
    , workout_user_id :: Int
    , workout_workout_type :: Text
    , workout_exercises :: [Exercise]
    , workout_runs :: [Run]
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

data Exercise = Exercise
    { exercise_id :: Int
    , exercise_reps :: Int
    , exercise_sets :: Int
    , exercise_workout_id :: Int
    , exercise_time :: Double
    , exercise_Lift :: Lift
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

data Run = Run
    { run_id :: Int
    , run_distance :: Float
    , run_time :: Double
    , run_mile_avg :: Float
    , run_speed_avg :: Float
    , run_workout_id :: Int
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

data Lift = Lift
    { lift_id :: Int
    , lift_name :: Text
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)