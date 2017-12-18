module Views.AddWorkout exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Dropdown as Drop
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Modal as Modal
import FontAwesome.Web as Icon
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Round exposing (..)
import Shared.Generated exposing (Exercise, Run, Set, User, Workout)
import Shared.Helper exposing (datePicker, displayMileAvg, displayTime)
import Types exposing (Cardio(..), Data(..), Model, Msg(..), SetFormMsg(..))


view : Model -> Html Msg
view model =
    let
        user =
            model.user

        wk =
            model.staged.stagedWorkout
    in
    div [ class "container" ]
        [ h1 [] [ text <| "Workout Name: " ++ wk.workout_name ]
        , div [ class "row run-row" ] <|
            List.append (List.map runView wk.workout_runs) <|
                List.append
                    (List.map (exerciseView True) <| List.indexedMap (,) wk.workout_exercises)
                    [ div [ class "row full-width" ]
                        [ div [ class "col-6" ]
                            [ Button.button
                                [ Button.onClick AddExButton
                                , Button.attrs [ class "half-screen-button add-ex-btn icon-with-text" ]
                                ]
                                [ Icon.hand_grab_o
                                , text "Add exercise"
                                ]
                            ]
                        , div [ class "col-6" ]
                            [ Button.button [ Button.onClick OpenCardio, Button.attrs [ class "half-screen-button add-cardio-btn icon-with-text" ] ]
                                [ Icon.heart
                                , text "Add Cardio"
                                ]
                            ]
                        , cardioModal model

                        -- , exerciseModal model
                        , setModal model
                        ]
                    ]
        , div [ class "save-workout-button row" ]
            [ Button.button [ Button.onClick SaveWorkout, Button.success, Button.attrs [ class "save-workout" ] ] [ text "Save Workout" ]
            , div [ class "saved-icon" ] <|
                case model.staged.fetching of
                    Fetching ->
                        [ text "Saving..." ]

                    Saved ->
                        [ Icon.check, text " Saved" ]

                    NotSaved ->
                        [ Icon.times, text " Not Saved" ]

                    Back ->
                        []
            ]
        ]


exerciseView : Bool -> ( Int, Exercise ) -> Html Msg
exerciseView bool ( index, ex ) =
    div [ class "col-12 run" ]
        [ Card.config [ Card.attrs [ class "run-card" ] ]
            |> Card.headerH1 []
                [ div [ class "ex-header" ]
                    [ text <| "Exercise" ++ toString (index + 1)
                    ]
                ]
            |> Card.block []
                [ Card.custom <| div [] <| List.map setView (List.indexedMap (,) ex.exercise_sets)
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


runView : Run -> Html Msg
runView run =
    div [ class "col-12 run" ]
        [ Card.config [ Card.attrs [ class "run-card" ] ]
            |> Card.headerH1 []
                [ div [ class "ex-header" ]
                    [ text "Cardio"
                    ]
                ]
            |> Card.block []
                [ Card.custom
                    (div
                        [ class "row" ]
                        [ div [ class "col-3" ] [ text <| "Distance: " ++ toString run.run_distance ]
                        , div [ class "col-3" ] [ text <| "Total time of Run: " ++ displayTime run.run_time ]
                        , div [ class "col-3" ] [ text <| "Mile Average " ++ displayMileAvg run.run_mile_avg ++ " Per Mile" ]
                        , div [ class "col-3" ] [ text <| "Speed Average: " ++ Round.round 2 run.run_speed_avg ++ " Mph" ]
                        ]
                    )
                ]
            |> Card.view
        ]


setView : ( Int, Set ) -> Html Msg
setView ( index, set ) =
    Card.config [ Card.attrs [ class "run-card" ] ]
        |> Card.block []
            [ Card.custom <|
                div [ class "row" ]
                    [ div [ class "col-2" ]
                        [ text <|
                            "Set: "
                                ++ toString (index + 1)
                        ]
                    , div [ class "col-2" ]
                        [ text <|
                            "Lift: "
                                ++ set.set_lift.lift_name
                        ]
                    , div [ class "col-4" ] [ text <| "Weight: " ++ toString set.set_weight ]
                    , div [ class "col-4" ] [ text <| "Reps: " ++ toString set.set_reps ]
                    ]
            ]
        |> Card.view



--map over list of sets


cardioModal : Model -> Html Msg
cardioModal model =
    Modal.config CardioModalMsg
        |> Modal.large
        |> Modal.h5 [] [ text "Add Run" ]
        |> Modal.body []
            [ div []
                [ div [ class "row" ]
                    [ span [ class "col-2" ] [ text "Distance: " ]
                    , Html.map CardioMsg <| Input.number [ Input.onInput DistanceInput, Input.attrs [ class "col-10" ] ]
                    ]
                , br [] []
                , div [ class "row" ]
                    [ span [ class "col-2" ] [ text "Time Of Run: " ]
                    , Html.map CardioMsg <| Input.number [ Input.onInput TimeInput, Input.attrs [ class "col-10" ] ]
                    ]
                , br [] []
                , div [ class "row" ]
                    [ Button.button [ Button.success, Button.onClick (CardioMsg AddRunButton), Button.attrs [ class "col-6" ] ] [ text "Add Run to Workout" ]
                    , Button.button [ Button.danger, Button.onClick Cancel, Button.attrs [ class "col-6" ] ] [ text "Cancel" ]
                    ]
                ]
            ]
        |> Modal.view model.cardioModal



-- exerciseModal : Model -> Html Msg
-- exerciseModal model =
--     Modal.config ExerciseModalMsg
--         |> Modal.large
--         |> Modal.h5 [] [ text "Add Run" ]
--         |> Modal.body []
--             [ div []
--                 [ div [ class "row" ]
--                     [ span [ class "col-2" ] [ text "Total Time Of Exercise: " ]
--                     , Html.map ExerciseMsg <| Input.number [ Input.onInput TimeOfEx, Input.attrs [ class "col-10" ] ]
--                     ]
--                 , br [] []
--                 , div [ class "row" ]
--                     [ Button.button [ Button.onClick (ExerciseMsg AddExButton), Button.success, Button.attrs [ class "col-6" ] ] [ text "Add Exercise to Workout" ]
--                     , Button.button [ Button.onClick Cancel, Button.danger, Button.attrs [ class "col-6" ] ] [ text "Cancel" ]
--                     ]
--                 ]
--             ]
--         |> Modal.view model.exerciseModal


addModal : Model -> Html Msg
addModal model =
    div []
        [ button [ class "add-workout-circle-button", onClick OpenAdd ] [ Icon.plus ]
        , Modal.config AddWorkoutModalMsg
            |> Modal.large
            |> Modal.h5 [] [ text "Add Run" ]
            |> Modal.body []
                [ div []
                    [ div [ class "row" ]
                        [ span [ class "col-2" ] [ text "Name of Workout: " ]
                        , Input.text [ Input.onInput NameInput, Input.attrs [ class "col-10" ] ]
                        ]
                    , br [] []
                    , div [ class "row" ]
                        [ Button.button [ Button.onClick AddWorkoutModalButton, Button.success, Button.attrs [ class "col-6" ] ] [ text "Add Workout" ]
                        , Button.button [ Button.onClick Cancel, Button.danger, Button.attrs [ class "col-6" ] ] [ text "Cancel" ]
                        ]
                    ]
                ]
            |> Modal.view model.addModal
        ]


setModal : Model -> Html Msg
setModal model =
    Modal.config SetModalMsg
        |> Modal.large
        |> Modal.h5 [] [ text "Add Set" ]
        |> Modal.body []
            [ div []
                [ div [ class "row" ]
                    [ span [ class "col-2" ] [ text "Lift: " ]
                    , Drop.dropdown
                        model.setDrop
                        { options = []
                        , toggleMsg = SetDrop
                        , toggleButton =
                            Drop.toggle [ Button.primary ] [ text "Choose A Lift" ]
                        , items = List.map (\x -> Drop.buttonItem [ onClick (SetMsg (LiftInput x)) ] [ text x.lift_name ]) model.lifts
                        }
                    , div []
                        [ text <|
                            "Selected: "
                                ++ model.staged.stagedSet.set_lift.lift_name
                        ]
                    ]
                , br [] []
                , div [ class "row" ]
                    [ span [ class "col-2" ] [ text "Weight: " ]
                    , Html.map SetMsg <| Input.number [ Input.onInput WeightInput, Input.attrs [ class "col-10" ] ]
                    ]
                , br []
                    []
                , div
                    [ class "row" ]
                    [ span [ class "col-2" ] [ text "Reps: " ]
                    , Html.map SetMsg <| Input.number [ Input.onInput RepInput, Input.attrs [ class "col-10" ] ]
                    ]
                , br [] []
                , div [ class "row" ]
                    [ Button.button [ Button.onClick (SetMsg AddSetButton), Button.success, Button.attrs [ class "col-6" ] ] [ text "Add Set to Exercise" ]
                    , Button.button [ Button.onClick Cancel, Button.danger, Button.attrs [ class "col-6" ] ] [ text "Cancel" ]
                    ]
                ]
            ]
        |> Modal.view model.setModal
