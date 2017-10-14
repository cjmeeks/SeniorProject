module Shared.Generated exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Json.Encode
import Http
import String
import Dict exposing (Dict)

type alias Workout =
    { workout_id : Int
    , workout_date : String
    , workout_total_time : Float
    , workout_type : String
    , workout_user_id : Int
    , workout_exercises : List (Exercise)
    , workout_runs : List (Run)
    }

encodeWorkout : Workout -> Json.Encode.Value
encodeWorkout x =
    Json.Encode.object
        [ ( "workout_id", Json.Encode.int x.workout_id )
        , ( "workout_date", Json.Encode.string x.workout_date )
        , ( "workout_total_time", Json.Encode.float x.workout_total_time )
        , ( "workout_type", Json.Encode.string x.workout_type )
        , ( "workout_user_id", Json.Encode.int x.workout_user_id )
        , ( "workout_exercises", (Json.Encode.list << List.map encodeExercise) x.workout_exercises )
        , ( "workout_runs", (Json.Encode.list << List.map encodeRun) x.workout_runs )
        ]

decodeWorkout : Decoder Workout
decodeWorkout =
    decode Workout
        |> required "workout_id" int
        |> required "workout_date" string
        |> required "workout_total_time" float
        |> required "workout_type" string
        |> required "workout_user_id" int
        |> required "workout_exercises" (list decodeExercise)
        |> required "workout_runs" (list decodeRun)

type alias Exercise =
    { exercise_id : Int
    , exercise_time : Int
    , exercise_workout_id : Int
    , exercise_sets : List (Set)
    }

encodeExercise : Exercise -> Json.Encode.Value
encodeExercise x =
    Json.Encode.object
        [ ( "exercise_id", Json.Encode.int x.exercise_id )
        , ( "exercise_time", Json.Encode.int x.exercise_time )
        , ( "exercise_workout_id", Json.Encode.int x.exercise_workout_id )
        , ( "exercise_sets", (Json.Encode.list << List.map encodeSet) x.exercise_sets )
        ]

decodeExercise : Decoder Exercise
decodeExercise =
    decode Exercise
        |> required "exercise_id" int
        |> required "exercise_time" int
        |> required "exercise_workout_id" int
        |> required "exercise_sets" (list decodeSet)

type alias Run =
    { run_id : Int
    , run_distance : Float
    , run_time : Float
    , run_mile_avg : Float
    , run_speed_avg : Float
    , run_workout_id : Int
    }

encodeRun : Run -> Json.Encode.Value
encodeRun x =
    Json.Encode.object
        [ ( "run_id", Json.Encode.int x.run_id )
        , ( "run_distance", Json.Encode.float x.run_distance )
        , ( "run_time", Json.Encode.float x.run_time )
        , ( "run_mile_avg", Json.Encode.float x.run_mile_avg )
        , ( "run_speed_avg", Json.Encode.float x.run_speed_avg )
        , ( "run_workout_id", Json.Encode.int x.run_workout_id )
        ]

decodeRun : Decoder Run
decodeRun =
    decode Run
        |> required "run_id" int
        |> required "run_distance" float
        |> required "run_time" float
        |> required "run_mile_avg" float
        |> required "run_speed_avg" float
        |> required "run_workout_id" int

type alias User =
    { user_id : Int
    , user_first_name : String
    , user_last_name : String
    , user_username : String
    , user_weight : Int
    , user_height : Int
    , user_age : Int
    , user_workouts : List (Workout)
    }

encodeUser : User -> Json.Encode.Value
encodeUser x =
    Json.Encode.object
        [ ( "user_id", Json.Encode.int x.user_id )
        , ( "user_first_name", Json.Encode.string x.user_first_name )
        , ( "user_last_name", Json.Encode.string x.user_last_name )
        , ( "user_username", Json.Encode.string x.user_username )
        , ( "user_weight", Json.Encode.int x.user_weight )
        , ( "user_height", Json.Encode.int x.user_height )
        , ( "user_age", Json.Encode.int x.user_age )
        , ( "user_workouts", (Json.Encode.list << List.map encodeWorkout) x.user_workouts )
        ]

decodeUser : Decoder User
decodeUser =
    decode User
        |> required "user_id" int
        |> required "user_first_name" string
        |> required "user_last_name" string
        |> required "user_username" string
        |> required "user_weight" int
        |> required "user_height" int
        |> required "user_age" int
        |> required "user_workouts" (list decodeWorkout)

type alias Lift =
    { lift_id : Int
    , lift_name : String
    }

encodeLift : Lift -> Json.Encode.Value
encodeLift x =
    Json.Encode.object
        [ ( "lift_id", Json.Encode.int x.lift_id )
        , ( "lift_name", Json.Encode.string x.lift_name )
        ]

decodeLift : Decoder Lift
decodeLift =
    decode Lift
        |> required "lift_id" int
        |> required "lift_name" string

type alias Set =
    { set_id : Int
    , set_weight : Int
    , set_reps : Int
    , set_exercise_id : Int
    , set_lift_id : Int
    , set_lift : Maybe (Lift)
    }

encodeSet : Set -> Json.Encode.Value
encodeSet x =
    Json.Encode.object
        [ ( "set_id", Json.Encode.int x.set_id )
        , ( "set_weight", Json.Encode.int x.set_weight )
        , ( "set_reps", Json.Encode.int x.set_reps )
        , ( "set_exercise_id", Json.Encode.int x.set_exercise_id )
        , ( "set_lift_id", Json.Encode.int x.set_lift_id )
        , ( "set_lift", (Maybe.withDefault Json.Encode.null << Maybe.map encodeLift) x.set_lift )
        ]

decodeSet : Decoder Set
decodeSet =
    decode Set
        |> required "set_id" int
        |> required "set_weight" int
        |> required "set_reps" int
        |> required "set_exercise_id" int
        |> required "set_lift_id" int
        |> required "set_lift" (maybe decodeLift)

getApiUser : Maybe (Int) -> Http.Request (User)
getApiUser query_userid =
    let
        params =
            List.filter (not << String.isEmpty)
                [ query_userid
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "userid=")
                    |> Maybe.withDefault ""
                ]
    in
        Http.request
            { method =
                "GET"
            , headers =
                []
            , url =
                String.join "/"
                    [ "/"
                    , "api"
                    , "user"
                    ]
                ++ if List.isEmpty params then
                       ""
                   else
                       "?" ++ String.join "&" params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson decodeUser
            , timeout =
                Nothing
            , withCredentials =
                False
            }

postAddWorkout : Maybe (String) -> Maybe (Float) -> Maybe (String) -> Maybe (Int) -> Http.Request (String)
postAddWorkout query_date query_total_time query_workout_type query_user_id =
    let
        params =
            List.filter (not << String.isEmpty)
                [ query_date
                    |> Maybe.map (Http.encodeUri >> (++) "date=")
                    |> Maybe.withDefault ""
                , query_total_time
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "total_time=")
                    |> Maybe.withDefault ""
                , query_workout_type
                    |> Maybe.map (Http.encodeUri >> (++) "workout_type=")
                    |> Maybe.withDefault ""
                , query_user_id
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "user_id=")
                    |> Maybe.withDefault ""
                ]
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                String.join "/"
                    [ "/"
                    , "add"
                    , "workout"
                    ]
                ++ if List.isEmpty params then
                       ""
                   else
                       "?" ++ String.join "&" params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson string
            , timeout =
                Nothing
            , withCredentials =
                False
            }

postAddExercise : Maybe (Int) -> Maybe (Int) -> Http.Request (String)
postAddExercise query_time query_workout_id =
    let
        params =
            List.filter (not << String.isEmpty)
                [ query_time
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "time=")
                    |> Maybe.withDefault ""
                , query_workout_id
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "workout_id=")
                    |> Maybe.withDefault ""
                ]
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                String.join "/"
                    [ "/"
                    , "add"
                    , "exercise"
                    ]
                ++ if List.isEmpty params then
                       ""
                   else
                       "?" ++ String.join "&" params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson string
            , timeout =
                Nothing
            , withCredentials =
                False
            }

postAddRun : Maybe (Float) -> Maybe (Float) -> Maybe (Float) -> Maybe (Float) -> Maybe (Int) -> Http.Request (String)
postAddRun query_distance query_time query_mile_avg query_speed_avg query_workout_id =
    let
        params =
            List.filter (not << String.isEmpty)
                [ query_distance
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "distance=")
                    |> Maybe.withDefault ""
                , query_time
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "time=")
                    |> Maybe.withDefault ""
                , query_mile_avg
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "mile_avg=")
                    |> Maybe.withDefault ""
                , query_speed_avg
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "speed_avg=")
                    |> Maybe.withDefault ""
                , query_workout_id
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "workout_id=")
                    |> Maybe.withDefault ""
                ]
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                String.join "/"
                    [ "/"
                    , "add"
                    , "run"
                    ]
                ++ if List.isEmpty params then
                       ""
                   else
                       "?" ++ String.join "&" params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson string
            , timeout =
                Nothing
            , withCredentials =
                False
            }

postAddSet : Maybe (Int) -> Maybe (Int) -> Maybe (Int) -> Maybe (Int) -> Http.Request (String)
postAddSet query_weight query_reps query_exercise_id query_lift_id =
    let
        params =
            List.filter (not << String.isEmpty)
                [ query_weight
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "weight=")
                    |> Maybe.withDefault ""
                , query_reps
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "reps=")
                    |> Maybe.withDefault ""
                , query_exercise_id
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "exercise_id=")
                    |> Maybe.withDefault ""
                , query_lift_id
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "lift_id=")
                    |> Maybe.withDefault ""
                ]
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                String.join "/"
                    [ "/"
                    , "add"
                    , "set"
                    ]
                ++ if List.isEmpty params then
                       ""
                   else
                       "?" ++ String.join "&" params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson string
            , timeout =
                Nothing
            , withCredentials =
                False
            }

postDeleteWorkout : Maybe (Int) -> Maybe (Int) -> Http.Request (String)
postDeleteWorkout query_workout_id query_user_id =
    let
        params =
            List.filter (not << String.isEmpty)
                [ query_workout_id
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "workout_id=")
                    |> Maybe.withDefault ""
                , query_user_id
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "user_id=")
                    |> Maybe.withDefault ""
                ]
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                String.join "/"
                    [ "/"
                    , "delete"
                    , "workout"
                    ]
                ++ if List.isEmpty params then
                       ""
                   else
                       "?" ++ String.join "&" params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson string
            , timeout =
                Nothing
            , withCredentials =
                False
            }

postDeleteExercise : Maybe (Int) -> Maybe (Int) -> Http.Request (String)
postDeleteExercise query_exercise_id query_user_id =
    let
        params =
            List.filter (not << String.isEmpty)
                [ query_exercise_id
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "exercise_id=")
                    |> Maybe.withDefault ""
                , query_user_id
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "user_id=")
                    |> Maybe.withDefault ""
                ]
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                String.join "/"
                    [ "/"
                    , "delete"
                    , "exercise"
                    ]
                ++ if List.isEmpty params then
                       ""
                   else
                       "?" ++ String.join "&" params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson string
            , timeout =
                Nothing
            , withCredentials =
                False
            }

postDeleteRun : Maybe (Int) -> Maybe (Int) -> Http.Request (String)
postDeleteRun query_run_id query_user_id =
    let
        params =
            List.filter (not << String.isEmpty)
                [ query_run_id
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "run_id=")
                    |> Maybe.withDefault ""
                , query_user_id
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "user_id=")
                    |> Maybe.withDefault ""
                ]
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                String.join "/"
                    [ "/"
                    , "delete"
                    , "run"
                    ]
                ++ if List.isEmpty params then
                       ""
                   else
                       "?" ++ String.join "&" params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson string
            , timeout =
                Nothing
            , withCredentials =
                False
            }

postDeleteSet : Maybe (Int) -> Maybe (Int) -> Http.Request (String)
postDeleteSet query_set_id query_user_id =
    let
        params =
            List.filter (not << String.isEmpty)
                [ query_set_id
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "set_id=")
                    |> Maybe.withDefault ""
                , query_user_id
                    |> Maybe.map (toString >> Http.encodeUri >> (++) "user_id=")
                    |> Maybe.withDefault ""
                ]
    in
        Http.request
            { method =
                "POST"
            , headers =
                []
            , url =
                String.join "/"
                    [ "/"
                    , "delete"
                    , "set"
                    ]
                ++ if List.isEmpty params then
                       ""
                   else
                       "?" ++ String.join "&" params
            , body =
                Http.emptyBody
            , expect =
                Http.expectJson string
            , timeout =
                Nothing
            , withCredentials =
                False
            }