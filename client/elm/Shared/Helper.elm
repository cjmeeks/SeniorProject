module Shared.Helper exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Shared.Generated exposing (User, Workout, Run, Exercise, Set, Lift)
import List.Extra exposing (..)
import Round


putTogetherDates : String -> String -> String
putTogetherDates start end =
    start ++ " TO " ++ end


datePicker : String -> Html msg
datePicker name =
    div [ class "row form-group" ]
        [ text "Date Range:"
        , input
            [ Html.Attributes.name name ]
            []
        ]


getRunStats : Run -> Run
getRunStats run =
    let
        timeHr =
            run.run_time / 3600

        mileAvg =
            (run.run_time / run.run_distance) / 3600

        speedAvg =
            run.run_distance / timeHr
    in
        { run | run_mile_avg = mileAvg, run_speed_avg = speedAvg }


displayTime : Float -> String
displayTime fl =
    let
        min =
            fl / 60

        sec =
            fl - (min * 60)
    in
        (toString min) ++ ":" ++ (toString sec)


displayMileAvg : Float -> String
displayMileAvg int =
    let
        hello =
            int * 60

        split =
            String.split "." <| Round.round 2 hello

        minutes =
            Maybe.withDefault "00" <| split !! 0

        sec =
            case String.toFloat <| Maybe.withDefault "0.0" <| (split !! 1) of
                Err msg ->
                    "error"

                Ok val ->
                    Round.round 0 ((val / 100) * 60)
    in
        minutes ++ ":" ++ sec


getTotalTimeWorkout : Workout -> Workout
getTotalTimeWorkout wk =
    let
        runs =
            wk.workout_runs

        exs =
            wk.workout_exercises

        totalEx =
            List.foldl (\x y -> x.exercise_time + y) 0 exs

        totalRun =
            List.foldl (\x y -> x.run_time + y) 0 runs
    in
        { wk | workout_total_time = toFloat totalEx + totalRun }


getGraphDataRunDistance : List Workout -> List ( Float, Float )
getGraphDataRunDistance wk =
    let
        allRuns =
            List.indexedMap (,) <| List.map .run_distance <| List.concat <| List.map .workout_runs wk
    in
        List.map (\( x, y ) -> ( toFloat x, y )) allRuns


getGraphDataRunSpeed : List Workout -> List ( Float, Float )
getGraphDataRunSpeed wk =
    let
        allRuns =
            List.indexedMap (,) <| List.map .run_speed_avg <| List.concat <| List.map .workout_runs wk
    in
        List.map (\( x, y ) -> ( toFloat x, y )) allRuns


getGraphDataRunMile : List Workout -> List ( Float, Float )
getGraphDataRunMile wk =
    let
        allRuns =
            List.indexedMap (,) <| List.map (\x -> x.run_mile_avg * 60) <| List.concat <| List.map .workout_runs wk
    in
        List.map (\( x, y ) -> ( toFloat x, y )) allRuns


getTotalWeight : List Workout -> Int
getTotalWeight wk =
    let
        exercises =
            List.concat <| List.map .workout_exercises wk

        sets =
            List.concat <| List.map .exercise_sets exercises

        weights =
            List.map (\x -> x.set_weight * x.set_reps) sets
    in
        List.foldl (+) 0 weights


getTotalDistance : List Workout -> Float
getTotalDistance wk =
    let
        runs =
            List.concat <| List.map .workout_runs wk

        distances =
            List.map .run_distance runs
    in
        List.foldl (+) 0 distances


getNumberSets : List Workout -> Int
getNumberSets wk =
    let
        sets =
            List.concat <| List.map .exercise_sets <| List.concat <| List.map .workout_exercises wk
    in
        List.length sets


getNumberExercises : List Workout -> Int
getNumberExercises wk =
    let
        exercises =
            List.concat <| List.map .workout_exercises wk
    in
        List.length exercises


getNumberRuns : List Workout -> Int
getNumberRuns wk =
    let
        runs =
            List.concat <| List.map .workout_runs wk
    in
        List.length runs


getAvgSpeed : List Workout -> Float
getAvgSpeed wk =
    let
        runs =
            List.concat <| List.map .workout_runs wk

        speeds =
            List.map .run_speed_avg runs

        total =
            List.foldl (+) 0 speeds
    in
        case List.length speeds of
            0 ->
                0

            _ ->
                (total / toFloat (List.length speeds))
