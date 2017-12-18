{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Api.User.Handler
    ( userHandler
    , loginHandler
    , singupHandler
    ) where

import Servant (Handler, err500,err400, errBody)        
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Error.Class(throwError)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.List (length)
import System.Random (getStdRandom, randomR)
import Database.PostgreSQL.Simple (Connection, Query, close, connect, execute, query)
import Database.PostgreSQL.Simple.SqlQQ
import Api.Types (User(..), Workout(..), Run(..), Set(..), Exercise(..), Lift(..), Saved(..))
import Api.Config (defaultConfig, DbConfig(..))
import System.IO.Unsafe (unsafePerformIO)
import Data.Text (Text)

userHandler :: Maybe Int -> Handler User
userHandler userid = do
    let userid' = fromMaybe 1 userid
    conn <- liftIO $ connect (postConfig defaultConfig)
    users <- getUser conn userid'
    let user = if length users > 0 then (Just (return $ users !! 0)) else Nothing
    newUser <- fromMaybe (throwError err500) user
    workouts <- liftIO $ getWorkouts conn (user_id newUser)
    something <- liftIO $ mapM (getRuns conn) workouts
    exercises <- liftIO $ mapM (getExercises conn) something
    let finalUser = newUser { user_workouts = exercises }
    _ <- liftIO $ close conn
    getSavedWorkouts finalUser

getUser ::  Connection -> Int -> Handler [User]
getUser conn userid = do
    user <- liftIO $ (query conn queryUser [userid] :: IO [User])
    case length user of
        0 -> throwError $ err400 {errBody = "(workout delete) No user with id"}
        _ -> return user

loginHandler :: Maybe Text -> Maybe Text -> Handler User
loginHandler username password = do
    let username' = fromMaybe "no" username
        password' = fromMaybe "no" password
    conn <- liftIO $ connect (postConfig defaultConfig)
    users <- liftIO $ (query conn loginQuery [username', password'] :: IO [User])
    let user = if length users > 0 then (Just (return $ users !! 0)) else Nothing
    newUser <- fromMaybe (throwError err500) user
    workouts <- liftIO $ getWorkouts conn (user_id newUser)
    something <- liftIO $ mapM (getRuns conn) workouts
    exercises <- liftIO $ mapM (getExercises conn) something
    let finalUser = newUser { user_workouts = exercises }
    _ <- liftIO $ close conn
    getSavedWorkouts finalUser

singupHandler :: Maybe Text -> User -> Handler Text
singupHandler username user = do
    conn <- liftIO $ connect (postConfig defaultConfig)
    numRows <- liftIO $ (execute conn signupQuery ((user_first_name user), (user_last_name user),(user_username user), (user_password user), (user_weight user), (user_height user), (user_age user)))
    _ <- liftIO $ close conn
    case numRows of
        0 -> throwError $ err400 {errBody = "(workout delete) No user with id"}
        _ -> return "Success"

getWorkouts :: Connection -> Int -> IO [Workout]
getWorkouts conn userid = do
    query conn queryWorkouts [userid] :: IO [Workout]

getWorkoutsSaved :: Connection -> Int -> Int -> IO Workout
getWorkoutsSaved conn userid wid= do
    lol <- query conn queryWorkoutsSaved [userid, wid] :: IO [Workout]
    yolo <- putStrLn $ show userid
    yolo' <- putStrLn $ show wid
    case length lol of
        0 -> return $ Workout (-1) "no" (-1) "no" (-1) [] []
        _ -> return $ lol !! 0

getRuns :: Connection -> Workout -> IO Workout
getRuns conn wk = do
    runs <- liftIO $ query conn queryRuns [workout_id wk] :: IO [Run]
    return $ wk { workout_runs = runs  }

getExercises :: Connection -> Workout -> IO Workout
getExercises conn wk = do
    exercises <- query conn queryExercises [workout_id wk] :: IO [Exercise]
    withSets <- mapM (getSets conn) exercises
    return $ wk { workout_exercises = withSets }

getSets :: Connection -> Exercise -> IO Exercise
getSets conn ex = do
    sets <- liftIO $ query conn querySets [exercise_id ex] :: IO [Set]
    withLift <- mapM (getLifts conn) sets
    return $ ex { exercise_sets = withLift }

getLifts :: Connection -> Set -> IO Set
getLifts conn set = do
    lifts <- liftIO $ query conn queryLifts [set_lift_id set] :: IO [Lift]
    return $ set { set_lift = lifts !! 0 }

getSavedWorkouts :: User -> Handler User
getSavedWorkouts user = do
    conn <- liftIO $ connect (postConfig defaultConfig)
    numRows <- liftIO $ (query conn getSavedQuery [user_id user] :: IO [Saved])
    saved <- liftIO $ mapM (\x -> getWorkoutsSaved conn (saved_workout x) (saved_user x)) numRows
    withRuns <- liftIO $ mapM (getRuns conn) saved
    withExercises <- liftIO $ mapM (getExercises conn) withRuns
    _ <- liftIO $ close conn
    case length numRows of
        0 -> return user
        _ -> return $ user {user_saved_workouts = withExercises}

getSavedQuery :: Query
getSavedQuery =
    [sql|
        SELECT * FROM workout.saved_workouts where user_id = ?
    |]
    
queryUser :: Query
queryUser =
    [sql|
        SELECT * FROM workout.users WHERE workout.users.user_id = ?
    |]

queryWorkoutsSaved :: Query
queryWorkoutsSaved =
    [sql|
        SELECT * FROM workout.workouts WHERE workout_id = ? AND user_id = ?
    |]
queryWorkouts :: Query
queryWorkouts =
    [sql|
        SELECT * FROM workout.workouts WHERE user_id = ?
    |]
queryRuns :: Query
queryRuns =
    [sql|
        SELECT * FROM workout.run WHERE workout_id = ?
    |]

queryExercises :: Query
queryExercises =
    [sql|
        SELECT * FROM workout.exercise WHERE workout_id = ?
    |]
querySets :: Query
querySets =
    [sql|
        SELECT * FROM workout.sets WHERE exercise_id = ?
    |]

queryLifts :: Query
queryLifts =
    [sql|
        SELECT * FROM workout.lifts WHERE lift_id = ?
    |]

loginQuery :: Query
loginQuery =
    [sql|
        SELECT * FROM workout.users WHERE username = ? AND password = ?
    |]

signupQuery :: Query
signupQuery =
    [sql|
        INSERT INTO workout.users (first_name, last_name, username, password, weight, height, age) VALUES (?, ?, ?, ?, ?, ?, ?)
    |]