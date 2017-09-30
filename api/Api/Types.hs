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
    , Workout(..)
    , Exercise(..)
    , Run(..)
    , User(..)
    , Lift(..)
    , Set(..)
    , ElmUTCTime(..)
    ) where

import Servant ((:<|>), (:>), Post, JSON, ReqBody, Raw, QueryParam, Get)
import Api.App.Types (Dice)
import Api.Elm (ElmUTCTime(..))
import Elm (ElmType(..))
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Data.UUID (UUID, toText)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)

type Api
        = "api"
            :> ("user" :> QueryParam "userid" Int
                           :> Get '[JSON] User
               )

type ApiWithAssets = (Api :<|> Raw)


data User = User
    { user_id :: Int
    , user_first_name :: Text
    , user_last_name :: Text
    , user_username :: Text
    , user_weight :: Int
    , user_height :: Int
    , user_age ::Int
    , user_workouts :: [Workout]
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

instance FromRow User where
    fromRow = do
        user_id' <- field 
        user_first_name' <- field 
        user_last_name' <- field 
        user_username' <- field 
        user_weight' <- field 
        user_height' <- field 
        user_age' <- field
        return $ User user_id' user_first_name' user_last_name' user_username' user_weight' user_height' user_age' []



data Workout = Workout
    { workout_id :: Int
    , workout_date :: ElmUTCTime
    , workout_total_time :: Double
    , workout_type :: Text
    , workout_user_id :: Int
    , workout_exercises :: [Exercise]
    , workout_runs :: [Run]
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

instance FromRow Workout where
    fromRow = do
        workout_id' <- field 
        workout_date' <- field 
        workout_total_time' <- field 
        workout_type' <- field 
        workout_user_id' <- field 
        return $ Workout workout_id' (ElmUTCTime workout_date') workout_total_time' workout_type' workout_user_id' [] []

data Exercise = Exercise
    { exercise_id :: Int
    , exercise_time :: Double
    , exercise_workout_id :: Int
    , exercise_sets :: [Set]
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

data Set = Set
    { set_id :: Int
    , set_weight :: Int
    , set_reps :: Int
    , set_exercise_id :: Int
    , set_lift_id :: Int
    , set_lift :: Maybe Lift
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

instance FromRow Set where
    fromRow = do
        set_id' <- field 
        set_weight' <- field 
        set_reps' <- field 
        set_exercise_id' <- field 
        set_lift_id' <- field 
        return $ Set set_id' set_weight' set_reps' set_exercise_id' set_lift_id' Nothing


data Run = Run
    { run_id :: Int
    , run_distance :: Double
    , run_time :: Double
    , run_mile_avg :: Double
    , run_speed_avg :: Double
    , run_workout_id :: Int
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

instance FromRow Run where
    fromRow = do
        run_id' <- field 
        run_distance' <- field 
        run_time' <- field 
        run_mile_avg' <- field 
        run_speed_avg' <- field 
        run_workout_id' <- field
        return $ Run run_id' run_distance' run_time' run_mile_avg' run_speed_avg' run_workout_id'

data Lift = Lift
    { lift_id :: Int
    , lift_name :: Text
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

instance FromRow Lift where
    fromRow = do
        lift_id' <- field 
        lift_name' <- field 
        return $ Lift lift_id' lift_name'