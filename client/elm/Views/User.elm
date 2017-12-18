module Views.User exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import FontAwesome.Web as Icon
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Shared.Generated exposing (User, Workout)
import Shared.Helper as Helper exposing (datePicker)
import Types exposing (Model, Msg(..))
import Views.Dashboard exposing (graphView)


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "" ]
            [ div [ class "col-11 user-profile" ] [ userView model.user ]
            ]

        -- , div [ class "settings-header" ] [ h1 [] [ text "Favorite Graphs" ] ]
        -- , div [ class "row main-row" ]
        --     [ div [ class "col-4" ]
        --         [ changeGraphView "name" ]
        --     , div [ class "col-4" ]
        --         [ changeGraphView "name" ]
        --     , div [ class "col-4" ]
        --         [ changeGraphView "name" ]
        --     ]
        , div [ class "settings-header" ] [ h1 [] [ text "Favorite Workouts" ] ]
        , div [ class "row main-row" ] <| List.map userWorkoutView model.user.user_saved_workouts
        ]


userView : User -> Html Msg
userView user =
    Card.config [ Card.attrs [ class "run-card" ] ]
        |> Card.headerH1 [ class "user-header" ]
            [ div [ class "profile-header" ]
                [ text (user.user_first_name ++ " " ++ user.user_last_name ++ " (" ++ user.user_username ++ ")")
                , div [ class "edit-profile" ]
                    [ Button.button [ Button.danger, Button.onClick Logout ] [ text "Logout" ] ]
                ]
            ]
        |> Card.block [ Card.blockAttrs [ class "row profile-row" ] ]
            [ Card.text [ class "col-4" ] [ label [] [ text "Weight: " ], text (" " ++ toString user.user_weight) ]
            , Card.text [ class "col-4" ] [ label [] [ text "Age: " ], text (" " ++ toString user.user_age) ]
            , Card.text [ class "col-4" ] [ label [] [ text "Height: " ], text (" " ++ toString user.user_height) ]
            ]
        |> Card.view


userWorkoutView : Workout -> Html Msg
userWorkoutView wk =
    div [ class "run-row col-12" ]
        [ Card.config [ Card.attrs [ class "run-card" ] ]
            |> Card.headerH1 []
                [ div [ class "ex-header row" ]
                    [ div [ class "col-1" ] [ text <| "Workout Name: " ++ wk.workout_name ]
                    , div [ class "col-10" ] [ Button.button [ Button.success, Button.onClick (AddSaved wk), Button.attrs [ class "delete-card-button" ] ] [ text "Add Workout" ] ]
                    , div [ class "col-1" ] [ Button.button [ Button.danger, Button.onClick (DeleteSavedWorkout wk.workout_id), Button.attrs [ class "delete-card-button" ] ] [ text "Delete" ] ]
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
        ]


changeGraphView : String -> Html Msg
changeGraphView str =
    Card.config [ Card.outlineInfo ]
        |> Card.headerH1 [ class "profile-header edit-graph-header" ]
            [ text str
            ]
        |> Card.view
