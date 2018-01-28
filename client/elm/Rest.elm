module Rest exposing (..)

import Http
import Shared.Generated
    exposing
        ( Exercise
        , Lift
        , Run
        , Set
        , User
        , Workout
        , getApiAddCurWID
        , getApiAddExercise
        , getApiAddRun
        , getApiAddSet
        , getApiLifts
        , getApiLogin
        , getApiUser
        , postApiAddSave
        , postApiAddWorkout
        , postApiDeleteSave
        , postApiSignup
        )
import Types exposing (HandleStuff(..), Msg(..))


getUser : Int -> Cmd Msg
getUser userId =
    Http.send processUser <| getApiUser (Just userId)


getLiftsCall : Cmd Msg
getLiftsCall =
    Http.send processLifts <| getApiLifts


getCurrentId : String -> Cmd Msg
getCurrentId str =
    case str of
        "w" ->
            Http.send (processGetCurrentId str) <| getApiAddCurWID

        "s" ->
            Http.send (processGetCurrentId str) <| getApiAddSet

        "r" ->
            Http.send (processGetCurrentId str) <| getApiAddRun

        "e" ->
            Http.send (processGetCurrentId str) <| getApiAddExercise

        _ ->
            Http.send (processGetCurrentId str) <| getApiAddCurWID


addWorkout : Int -> Workout -> Cmd Msg
addWorkout userid wk =
    Http.send processSaveWorkout <| postApiAddWorkout (Just userid) wk


saveWorkout : Int -> Workout -> Cmd Msg
saveWorkout userid wk =
    Http.send processUserSaveWorkout <| postApiAddSave (Just userid) (Just wk.workout_id)


loginCall : String -> String -> Cmd Msg
loginCall username pass =
    Http.send processLogin <| getApiLogin (Just username) (Just pass)


signupCall : User -> Cmd Msg
signupCall user =
    Http.send processSignup <| postApiSignup (Just "hello") user


deleteSavedWorkout : Int -> Cmd Msg
deleteSavedWorkout wid =
    Http.send processDelete <| postApiDeleteSave (Just wid)


processUser : Result Http.Error User -> Msg
processUser result =
    case result of
        Ok us ->
            UpdateHandle <| HandleUser us

        Err err ->
            UpdateHandle <| HandleError err


processLogin : Result Http.Error User -> Msg
processLogin result =
    case result of
        Ok us ->
            UpdateHandle <| HandleLogin us

        Err err ->
            UpdateHandle <| HandleError err


processLifts : Result Http.Error (List Lift) -> Msg
processLifts result =
    case result of
        Ok a ->
            UpdateHandle <| HandleLifts a

        Err err ->
            UpdateHandle <| HandleError err


processSaveWorkout : Result Http.Error User -> Msg
processSaveWorkout result =
    case result of
        Ok us ->
            UpdateHandle <| HandleSavedUser us

        Err err ->
            UpdateHandle <| HandleError err


processUserSaveWorkout : Result Http.Error String -> Msg
processUserSaveWorkout result =
    case result of
        Ok us ->
            UpdateHandle <| HandleSuccess us

        Err err ->
            UpdateHandle <| HandleError err


processGetCurrentId : String -> Result Http.Error Int -> Msg
processGetCurrentId y result =
    case result of
        Ok id ->
            UpdateHandle <| HandleCurrentId y id

        Err err ->
            UpdateHandle <| HandleError err


processDelete : Result Http.Error String -> Msg
processDelete result =
    case result of
        Ok us ->
            UpdateHandle <| HandleSuccess us

        Err err ->
            UpdateHandle <| HandleError err


processSignup : Result Http.Error String -> Msg
processSignup result =
    case result of
        Ok us ->
            UpdateHandle <| HandleSignup us

        Err err ->
            UpdateHandle <| HandleError err



-- addWorkout : Int -> Workout -> Cmd Msg
-- addWorkout user_id wk =
--     Http.send processUser <| postAddWorkout wk
