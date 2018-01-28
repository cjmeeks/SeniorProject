module Views.Top exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Dropdown as Drop
import Bootstrap.Modal as Modal
import Bootstrap.Navbar as Nav
import Dict
import FontAwesome.Web as Icon
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Init
    exposing
        ( currentIdCmds
        , initExercise
        , initLift
        , initLogin
        , initRun
        , initSet
        , initUser
        , initWorkout
        , initialModel
        )
import Navigation exposing (Location)
import Ports.Ports exposing (DatePickerMsg, buildDatePicker, getCurrentDate, handleDateChange)
import Rest
    exposing
        ( addWorkout
        , deleteSavedWorkout
        , getCurrentId
        , getLiftsCall
        , getUser
        , loginCall
        , saveWorkout
        , signupCall
        )
import Routing exposing (parseLocation)
import Shared.Generated exposing (encodeExercise, encodeLift, encodeRun, encodeSet, encodeUser, encodeWorkout)
import Shared.Helper exposing (getGraphDataRunDistance, getGraphDataRunMile, getGraphDataRunSpeed, getRunStats, getTotalTimeWorkout)
import Types exposing (Cardio(..), Data(..), Model, Msg(..), Page(..), SetFormMsg(..))
import Update.Update as U
import Views.AddWorkout as Add exposing (addModal)
import Views.Dashboard as Dash
import Views.Login as Login
import Views.Nav as NavView
import Views.Signup as Signup
import Views.Stats as Stats
import Views.User as User
import Views.Workout as Workout


view : Model -> Html Msg
view model =
    let
        curView =
            case model.currentPage of
                Dashboard ->
                    div [ class "main-row" ]
                        [ Dash.view model
                        , addModal model
                        ]

                AddWorkout ->
                    div [ class "main-row" ]
                        [ Add.view model
                        , addModal model
                        ]

                Workouts ->
                    div [ class "main-row" ]
                        [ Workout.view model
                        , addModal model
                        ]

                Stats ->
                    div [ class "main-row" ]
                        [ Stats.view model
                        ]

                Profile ->
                    div [ class "main-row" ]
                        [ User.view model
                        ]

                ViewWorkout ->
                    div [ class "main-row" ]
                        [ Workout.detailedWorkoutView model.selectedWorkout
                        ]

                SignUp ->
                    div [] []

                Login ->
                    div [ class "login-layout-grid" ] [ Login.view model ]
    in
    if model.currentPage == Login then
        div []
            [ curView
            , Signup.signupModal model
            ]
    else
        div []
            [ NavView.newView model
            , curView
            ]
