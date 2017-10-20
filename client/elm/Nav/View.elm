module Nav.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Types exposing (Model, Msg(..), Page(..))
import FontAwesome.Web as Icon


newView : Model -> Html Msg
newView model =
    let
        dashClass =
            if model.currentPage == Dashboard then
                "active-nav nav-item"
            else
                "not-active-nav nav-item"

        addClass =
            if model.currentPage == AddWorkout then
                "active-nav nav-item"
            else
                "not-active-nav nav-item"

        workClass =
            if model.currentPage == Workouts then
                "active-nav nav-item"
            else
                "not-active-nav nav-item"

        statClass =
            if model.currentPage == Stats then
                "active-nav nav-item"
            else
                "not-active-nav nav-item"

        profClass =
            if model.currentPage == Profile then
                "active-nav nav-item"
            else
                "not-active-nav nav-item"
    in
        nav []
            [ ul []
                [ li [ class <| dashClass ++ " icon-with-text" ] [ a [ href "#dashboard" ] [ Icon.dashboard, text "Dashboard" ] ]
                , li [ class <| addClass ++ " icon-with-text" ] [ a [ href "#addworkout" ] [ Icon.plus, text "Add Workout" ] ]
                , li [ class <| workClass ++ " icon-with-text" ] [ a [ href "#workouts" ] [ Icon.bars, text "Workouts" ] ]
                , li [ class <| statClass ++ " icon-with-text" ] [ a [ href "#stats" ] [ Icon.bar_chart, text "Stats" ] ]
                , li [ class <| profClass ++ " icon-with-text" ] [ a [ href "#profile" ] [ Icon.user, text "Profile" ] ]
                ]
            ]
