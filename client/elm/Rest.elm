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
        , postApiDeleteSave
        , postApiAddSave
        , getApiUser
        , postApiAddWorkout
        , getApiLifts
        , getApiAddCurWID
        , getApiAddExercise
        , getApiAddRun
        , getApiAddSet
        , getApiLogin
        , postApiSignup
        )
import Types exposing (Msg(..))


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
            HandleUser us

        Err err ->
            HandleError err


processLogin : Result Http.Error User -> Msg
processLogin result =
    case result of
        Ok us ->
            HandleLogin us

        Err err ->
            HandleError err


processLifts : Result Http.Error (List Lift) -> Msg
processLifts result =
    case result of
        Ok a ->
            HandleLifts a

        Err err ->
            HandleError err


processSaveWorkout : Result Http.Error User -> Msg
processSaveWorkout result =
    case result of
        Ok us ->
            HandleSavedUser us

        Err err ->
            HandleError err


processUserSaveWorkout : Result Http.Error String -> Msg
processUserSaveWorkout result =
    case result of
        Ok us ->
            HandleSuccess us

        Err err ->
            HandleError err


processGetCurrentId : String -> Result Http.Error Int -> Msg
processGetCurrentId y result =
    case result of
        Ok id ->
            HandleCurrentId y id

        Err err ->
            HandleError err


processDelete : Result Http.Error String -> Msg
processDelete result =
    case result of
        Ok us ->
            HandleSuccess us

        Err err ->
            HandleError err


processSignup : Result Http.Error String -> Msg
processSignup result =
    case result of
        Ok us ->
            HandleSignup us

        Err err ->
            HandleError err



-- addWorkout : Int -> Workout -> Cmd Msg
-- addWorkout user_id wk =
--     Http.send processUser <| postAddWorkout wk
