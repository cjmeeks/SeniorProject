module Dashboard.View exposing (..)

import Bootstrap.Card as Card
import FontAwesome.Web as Icon
import Html exposing (..)
import Html.Attributes exposing (..)
import Types exposing (Model, Msg(..))
import Workout.View exposing (workoutView)


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "row main-row" ]
            [ div [ class "test col-4 main-graph" ]
                [ graphView ]
            , div [ class "test col-4 main-graph" ]
                [ graphView ]
            , div [ class "test col-4 main-graph" ]
                [ graphView ]
            ]
        , div [ class "row run-row" ]
            [ div [ class "col-12 run" ] <|
                List.map workoutView model.user.user_workouts
            ]
        ]


graphView : Html Msg
graphView =
    Card.config []
        |> Card.headerH1 [] [ text "Graph" ]
        |> Card.block []
            [ Card.titleH1 [] [ text "Block title" ]
            , Card.text [] [ text "Some block content" ]
            ]
        |> Card.view
