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
    , Saved(..)
    ) where

import Servant ((:<|>), (:>), Post, JSON, ReqBody, Raw, QueryParam, Get, ReqBody)
import Api.Elm (ElmUTCTime(..))
import Elm (ElmType(..))
import Data.Aeson (FromJSON, ToJSON, (.!=), (.:), (.:?), eitherDecode, encode, parseJSON, withObject)
import Data.Serialize (Serialize)
import GHC.Generics (Generic)
import Data.Text (Text)
import Data.Time (Day, UTCTime)
import Data.UUID (UUID, toText)
import GHC.Generics (Generic)
import Database.PostgreSQL.Simple.FromRow (FromRow, field, fromRow)
import Web.HttpApiData (FromHttpApiData(..))
import Data.String.Conversions (cs)

type Api
        = "api"
            :> ("user"  :> QueryParam "userid" Int
                                :> Get '[JSON] User
            :<|> "login" :> QueryParam "username" Text
                            :> QueryParam "pass" Text
                             :> Get '[JSON] User
            :<|> "signup"  :> QueryParam "temp" Text
                            :> ReqBody '[JSON] User
                              :> Post '[JSON] Text
            :<|>  "lifts" :> Get '[JSON] [Lift] 
            :<|> "add" :> ("workout" :>  QueryParam "user_id" Int
                                        :> ReqBody '[JSON] Workout
                                        :> Post '[JSON] User
                            :<|> "exercise" :> Get '[JSON] Int
                            :<|> "run" :> Get '[JSON] Int
                            :<|> "set" :> Get '[JSON] Int
                            :<|> "curWID" :> Get '[JSON] Int
                            :<|> "save" :>  QueryParam "user_id" Int :>  QueryParam "workout_id" Int :> Post '[JSON] Text
                            )
            :<|> "delete" :> ( "workout" :> QueryParam "workout_id" Int
                                            :> QueryParam "user_id" Int
                                            :> Post '[JSON] Text
                            :<|> "exercise" :> QueryParam "exercise_id" Int
                                            :> QueryParam "user_id" Int
                                            :> Post '[JSON] Text
                            :<|> "run" :> QueryParam "run_id" Int
                                            :> QueryParam "user_id" Int
                                            :> Post '[JSON] Text
                            :<|> "set" :> QueryParam "set_id" Int
                                            :> QueryParam "user_id" Int
                                            :> Post '[JSON] Text
                            :<|> "save" :> QueryParam "workout_id" Int
                                        :> Post '[JSON] Text

                            )
            )

type ApiWithAssets = (Api :<|> Raw)


data User = User
    { user_id :: Int
    , user_first_name :: String
    , user_last_name :: String
    , user_username :: String
    , user_password :: String
    , user_weight :: Int
    , user_height :: Int
    , user_age ::Int
    , user_workouts :: [Workout]
    , user_saved_workouts :: [Workout]
    } deriving (Show, Generic, ElmType, ToJSON, FromJSON)

instance FromRow User where
    fromRow = do
        user_id' <- field 
        user_first_name' <- field 
        user_last_name' <- field 
        user_username' <- field
        user_password' <- field 
        user_weight' <- field 
        user_height' <- field 
        user_age' <- field
        return $ User user_id' user_first_name' user_last_name' user_username' user_password' user_weight' user_height' user_age' [] []

data Workout = Workout
    { workout_id :: Int
    , workout_date :: String
    , workout_total_time :: Double
    , workout_name :: String
    , workout_user_id :: Int
    , workout_exercises :: [Exercise]
    , workout_runs :: [Run]
    } deriving (Show, Generic, Eq, ElmType, ToJSON)

data Echo = Echo
    { a :: String
    } deriving (Show, Generic, ElmType,Eq, ToJSON, FromJSON)

instance FromHttpApiData Echo where
    parseQueryParam = either (Left . cs) (Right . Prelude.id) . eitherDecode . cs

instance FromRow Workout where
    fromRow = do
        workout_id' <- field 
        workout_date' <- field 
        workout_total_time' <- field 
        workout_name' <- field 
        workout_user_id' <- field 
        return $ Workout workout_id' workout_date' workout_total_time' workout_name' workout_user_id' [] []

instance FromJSON Workout where
    parseJSON =
        withObject "workout" $ \o -> do
            run1 <- o .:? "workout_id" .!= 0
            run2 <- o .:? "workout_date" .!= "2000-01-01"
            run3 <- o .:? "workout_total_time" .!= 0
            run4 <- o .:? "workout_name" .!= "no Name"
            run5 <- o .:? "workout_user_id" .!= 0
            run6 <- o .:? "workout_exercises" .!= []
            run7 <- o .:? "workout_runs" .!= []
            return $ Workout run1 run2 run3 run4 run5 run6 run7

instance FromHttpApiData Workout where
    parseQueryParam = either (Left . cs) (Right . Prelude.id) . eitherDecode . cs

instance Serialize Workout

data Exercise = Exercise
    { exercise_id :: Int
    , exercise_time :: Int
    , exercise_workout_id :: Int
    , exercise_sets :: [Set]
    } deriving (Show, Generic,Eq, ElmType, ToJSON)

instance FromRow Exercise where
    fromRow = do
        exercise_id' <- field 
        exercise_time' <- field 
        exercise_workout_id' <- field 
        return $ Exercise exercise_id' exercise_time' exercise_workout_id' []


instance FromJSON Exercise where
    parseJSON =
        withObject "exercise" $ \o -> do
            run1 <- o .:? "exercise_id" .!= 0
            run2 <- o .:? "exercise_time" .!= 0
            run3 <- o .:? "exercise_workout_id" .!= 0
            run4 <- o .:? "exercise_sets" .!= []
            return $ Exercise run1 run2 run3 run4

instance FromHttpApiData Exercise where
    parseQueryParam = either (Left . cs) (Right . Prelude.id) . eitherDecode . cs

instance Serialize Exercise

data Set = Set
    { set_id :: Int
    , set_weight :: Int
    , set_reps :: Int
    , set_exercise_id :: Int
    , set_lift_id :: Int
    , set_lift :: Lift
    } deriving (Show, Generic,Eq, ElmType, ToJSON)

instance FromRow Set where
    fromRow = do
        set_id' <- field 
        set_weight' <- field 
        set_reps' <- field 
        set_exercise_id' <- field 
        set_lift_id' <- field 
        return $ Set set_id' set_weight' set_reps' set_exercise_id' set_lift_id' initLift

instance FromJSON Set where
    parseJSON =
        withObject "set" $ \o -> do
            run1 <- o .:? "set_id" .!= 0
            run2 <- o .:? "set_weight" .!= 0
            run3 <- o .:? "set_reps" .!= 0
            run4 <- o .:? "set_exercise_id" .!= 0
            run5 <- o .:? "set_lift_id" .!= 0
            run6 <- o .: "set_lift"
            return $ Set run1 run2 run3 run4 run5 run6

instance FromHttpApiData Set where
    parseQueryParam = either (Left . cs) (Right . Prelude.id) . eitherDecode . cs

instance Serialize Set

data Run = Run
    { run_id :: Int
    , run_distance :: Double
    , run_time :: Double
    , run_mile_avg :: Double
    , run_speed_avg :: Double
    , run_workout_id :: Int
    } deriving (Show, Generic, Eq, ElmType, ToJSON)

instance FromRow Run where
    fromRow = do
        run_id' <- field 
        run_distance' <- field 
        run_time' <- field 
        run_mile_avg' <- field 
        run_speed_avg' <- field 
        run_workout_id' <- field
        return $ Run run_id' run_distance' run_time' run_mile_avg' run_speed_avg' run_workout_id'

instance FromJSON Run where
    parseJSON =
        withObject "run" $ \o -> do
            run1 <- o .:? "run_id" .!= 0
            run2 <- o .:? "run_distance" .!= 0
            run3 <- o .:? "run_time" .!= 0
            run4 <- o .:? "run_mile_avg" .!= 0
            run5 <- o .:? "run_speed_avg" .!= 0
            run6 <- o .:? "run_workout_id" .!= 0
            return $ Run run1 run2 run3 run4 run5 run6
instance FromHttpApiData Run where
    parseQueryParam = either (Left . cs) (Right . Prelude.id) . eitherDecode . cs

instance Serialize Run

data Lift = Lift
    { lift_id :: Int
    , lift_name :: String
    } deriving (Show, Generic,Eq, ElmType, ToJSON)

initLift :: Lift
initLift = Lift (-1) "noLift"

instance FromHttpApiData Lift where
    parseQueryParam = either (Left . cs) (Right . Prelude.id) . eitherDecode . cs

instance FromRow Lift where
    fromRow = do
        lift_id' <- field 
        lift_name' <- field 
        return $ Lift lift_id' lift_name'

instance FromJSON Lift where
    parseJSON =
        withObject "lift" $ \o -> do
            lift_id' <- o .:? "lift_id" .!= 0
            lift_name' <- o .:? "lift_name" .!= "unknown"
            return $ Lift lift_id' lift_name'

instance Serialize Lift

data Saved = Saved
    { saved_user :: Int
    , saved_workout :: Int
    } deriving (Show, Generic,Eq, ElmType, ToJSON, FromJSON)

instance FromRow Saved where
fromRow = do
    user' <- field 
    wk' <- field 
    return $ Saved user' wk'

instance Serialize Saved