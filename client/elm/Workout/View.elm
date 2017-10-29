module Workout.View exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import FontAwesome.Web as Icon
import Html exposing (..)
import Html.Attributes exposing (..)
import Shared.Helper exposing (datePicker)
import Types exposing (Model, Msg(..))


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "row run-row" ]
            [ div [ class "col-3 date-range" ] [ datePicker "wk-date-range" ]
            , div [ class "col-12 run" ] [ workoutView ]
            ]
        ]


workoutView : Html Msg
workoutView =
    Card.config [ Card.attrs [ class "run-card" ], Card.outlineInfo ]
        |> Card.headerH1 []
            [ div [ class "ex-header" ]
                [ text "Workout Info"
                ]
            ]
        |> Card.block []
            [ Card.custom summaryView
            ]
        |> Card.view


summaryView : Html Msg
summaryView =
    Card.config [ Card.attrs [ class "run-card" ], Card.outlineInfo ]
        |> Card.block []
            [ Card.text [] [ summary ]
            ]
        |> Card.view



--map over list of sets


summary : Html Msg
summary =
    div [] [ text "Summary" ]
