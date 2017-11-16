module Printable.View exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import FontAwesome.Web as Icon
import Html exposing (..)
import Html.Attributes exposing (..)
import Shared.Generated exposing (Workout, Exercise, Set, Run)
import Shared.Helper exposing (datePicker)
import Types exposing (Model, Msg(..))
import Init exposing (initLift)


view : Workout -> Html Msg
view wk =
    let
        exs =
            wk.workout_exercises

        runs =
            wk.workout_runs
    in
        div [ class "print-wrapper" ]
            List.append
            [ h1 [] [ text wk.workout_date ] ]
        <|
            List.append (List.map viewExercises exs) <|
                List.map viewRuns runs


viewExercises : Exercise -> Html Msg
viewExercises ex =
    div [ class "exercise-wrapper" ]
        List.append
        [ h3 [] [ text "Exercises:" ] ]
    <|
        List.map viewSets ex.exercise_sets


viewSets : Set -> Html Msg
viewSets set =
    let
        lift =
            Maybe.withDefault (initLift) set.set_lift
    in
        div [ class "set-wrapper" ]
            [ h5 [] [ text "Set:" ]
            , div [] [ text <| "Lift: " ++ lift.lift_name ]
            , div [] [ text <| "\tWeight: " ++ (toString set.set_weight) ]
            , div [] [ text <| "\tReps: " ++ (toString set.set_repes) ]
            ]


viewRuns : Run -> Html Msg
viewRuns run =
    div [ class "run-wrapper" ]
        [ h3 [] [ text "Runs:" ]
        , div [] [ text <| toString run.run_distance ]
        ]
