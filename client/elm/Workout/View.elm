module Workout.View exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import FontAwesome.Web as Icon
import Html exposing (..)
import Html.Attributes exposing (..)
import Shared.Generated exposing (Workout)
import Shared.Helper exposing (datePicker)
import Types exposing (Model, Msg(..))


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "row run-row" ]
            [ div [ class "col-3 date-range" ] [ datePicker "wk-date-range" ]
            , div [ class "col-12 run" ] <| List.map workoutView model.user.user_workouts
            ]
        ]


workoutView : Workout -> Html Msg
workoutView wk =
    Card.config [ Card.attrs [ class "run-card" ] ]
        |> Card.headerH1 []
            [ div [ class "ex-header" ]
                [ text "Workout Info"
                ]
            ]
        |> Card.block []
            [ Card.text [] [ text <| "Date: " ++ wk.workout_date ]
            , Card.text [] [ text <| "Total time of workout: " ++ toString wk.workout_total_time ]
            , Card.text [] [ text <| "Type: " ++ wk.workout_type ]
            , Card.text [] [ text <| "# Exercises: " ++ toString (List.length wk.workout_exercises) ]
            , Card.text [] [ text <| "# Runs: " ++ toString (List.length wk.workout_runs) ]
            ]
        |> Card.view
