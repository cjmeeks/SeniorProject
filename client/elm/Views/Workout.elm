module Views.Workout exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import FontAwesome.Web as Icon
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Shared.Generated exposing (Workout)
import Shared.Helper exposing (datePicker)
import Types exposing (Model, Msg(..))
import Views.AddWorkout exposing (exerciseView, runView)


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "row run-row" ]
            [ div [ class "col-12 my-workouts" ] [ text "My Workouts" ]
            , div [ class "col-12 run" ] <| List.map (workoutView True) model.user.user_workouts
            ]
        ]


workoutView : Bool -> Workout -> Html Msg
workoutView bool wk =
    Card.config [ Card.attrs [ class "run-card" ] ]
        |> Card.headerH1 []
            [ div [ class "ex-header row" ]
                [ div [ class "col-1" ] [ text <| "Workout Name: " ++ wk.workout_name ]
                , if bool then
                    div [ class "col-11" ] [ Button.button [ Button.success, Button.onClick (SaveUserWorkout wk), Button.attrs [ class "fav-wk-button" ] ] [ text "Favorite" ] ]
                  else
                    span [] []
                ]
            ]
        |> Card.block []
            [ Card.custom
                (div [ class "row view-workout-click", onClick (ViewWorkoutClick wk) ]
                    [ div [ class "col-2" ] [ text <| "Date: " ++ wk.workout_date ]
                    , div [ class "col-2" ] [ text <| "Total time of workout: " ++ toString wk.workout_total_time ]
                    , div [ class "col-2" ] [ text <| "# Exercises: " ++ toString (List.length wk.workout_exercises) ]
                    , div [ class "col-2" ] [ text <| "# Runs: " ++ toString (List.length wk.workout_runs) ]
                    ]
                )
            ]
        |> Card.view


detailedWorkoutView : Workout -> Html Msg
detailedWorkoutView wk =
    div [ class "row run-row" ]
        [ div [ class "col-12" ] [ h1 [] [ text <| "Workout Name: " ++ wk.workout_name ] ]
        , div [ class "col-12" ]
            [ if List.length wk.workout_runs > 0 then
                h1 [] [ text "Cardio:" ]
              else
                h4 [] [ text "No Cardio" ]
            ]
        , div [ class "col-12" ] (List.map runView wk.workout_runs)
        , div [ class "col-12" ]
            [ if List.length wk.workout_exercises > 0 then
                h3 [] [ text "Exercises:" ]
              else
                h4 [] [ text "No Exercises" ]
            ]
        , div [ class "col-12" ] (List.map (exerciseView False) <| List.indexedMap (,) wk.workout_exercises)
        ]

printExerciseView :( Int, Exercise ) -> Html Msg
printExerciseView ( index, ex ) =
    div [ class "col-12 run" ]
        [ div [ class "ex-header" ]
                    [ text <| "Exercise" ++ toString (index + 1)
                    ]
                ]
        , div [] <| List.map setView (List.indexedMap (,) ex.exercise_sets)
                , if bool then
                    Card.custom <|
                        div [ class "row" ]
                            [ span [ class "col-4" ] []
                            , Button.button [ Button.onClick (OpenSet ex.exercise_id), Button.attrs [ class "col-4" ] ]
                                [ Icon.plus ]
                            ]
                  else
                    Card.custom <| span [] []
                ]
            |> Card.view
        ]
