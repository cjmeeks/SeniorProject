module Dashboard.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Bootstrap.Card as Card
import Types exposing (Model, Msg(..))
import FontAwesome.Web as Icon


view : Model -> Html Msg
view model = 
  div [class "container"]
    [ div [class "row main-row"]
        [ div [class "test col-4"]
            [ graphView ]
        , div [class "test col-4"]
            [ graphView ]
        , div [class "test col-4"]
            [ graphView ]
        ]
    , div [class "row run-row"]
        [ div [class "col-12 run"]
            [ workoutView ]
        , div [class "col-12 run"]
            [ workoutView ]
        , div [class "col-12 run"]
            [ workoutView ]
        , div [class "col-12 run"]
            [ workoutView ]
        ]
    ]

  
graphView : Html Msg
graphView =
  Card.config [ Card.outlineInfo ]
    |> Card.headerH1 [] [ text "Graph" ]
    |> Card.block []
        [ Card.titleH1 [] [ text "Block title" ]
        , Card.text [] [ text "Some block content" ]
        ]
    |> Card.view


workoutView : Html Msg
workoutView =
  Card.config [ Card.attrs [class "run-card"], Card.outlineInfo ]
    |> Card.headerH1 [] [ text "My Workout Info" ]
    |> Card.block []
        [ Card.text [] [ text "Some Workout content" ]
        ]
    |> Card.view