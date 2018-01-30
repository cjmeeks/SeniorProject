module Views.Top exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Styles.Styles exposing (..)
import Types exposing (Cardio(..), Data(..), Model, Msg(..), Page(..), SetFormMsg(..))
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
                    div []
                        [ Dash.view model
                        , addModal model
                        ]

                AddWorkout ->
                    div []
                        [ Add.view model
                        , addModal model
                        ]

                Workouts ->
                    div []
                        [ Workout.view model
                        , addModal model
                        ]

                Stats ->
                    div []
                        [ Stats.view model
                        ]

                Profile ->
                    div []
                        [ User.view model
                        ]

                ViewWorkout ->
                    div []
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
        div [ class "main-layout" ]
            [ div [ gridAccess 1 1 ] [ NavView.newView model ]
            , div [ gridAccess 2 1 ] [ curView ]
            ]
