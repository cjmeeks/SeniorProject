module Views.Stats exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Tab as Tab
import FontAwesome.Web as Icon
import Html exposing (..)
import Html.Attributes exposing (..)
import Shared.Generated exposing (User)
import Shared.Graph as G
import Shared.Helper as Helper exposing (datePicker)
import Types exposing (Graphs, Model, Msg(..))
import Views.Dashboard exposing (graphView)


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "col-12 profile-stats" ]
                [ profileStatsView model.user ]
            , div [ class "col-12" ]
                [ tabCard model ]
            ]
        ]


profileStatsView : User -> Html Msg
profileStatsView user =
    Card.config [ Card.attrs [ class "run-card" ] ]
        |> Card.headerH1 [ class "user-header" ]
            [ div [ class "profile-header" ]
                [ text "Profile Stats"
                ]
            ]
        |> Card.block [ Card.blockAttrs [ class "row profile-row" ] ]
            [ Card.text [ class "col-4" ] [ label [] [ text "Number of Workouts: " ], text (" " ++ (toString <| List.length user.user_workouts)) ]
            , Card.text [ class "col-4" ] [ label [] [ text "Total Number of Exercises: " ], text (" " ++ toString (Helper.getNumberExercises user.user_workouts)) ]
            , Card.text [ class "col-4" ] [ label [] [ text "Total Number of Runs: " ], text (" " ++ toString (Helper.getNumberRuns user.user_workouts)) ]
            ]
        |> Card.block [ Card.blockAttrs [ class "row profile-row" ] ]
            [ Card.text [ class "col-4" ] [ label [] [ text "Total Number of Sets: " ], text (" " ++ toString (Helper.getNumberSets user.user_workouts)) ]
            , Card.text [ class "col-4" ] [ label [] [ text "Total Weight Lifted: " ], text (" " ++ toString (Helper.getTotalWeight user.user_workouts)) ]
            , Card.text [ class "col-4" ] [ label [] [ text "Total Distance Run: " ], text (" " ++ toString (Helper.getTotalDistance user.user_workouts)) ]
            ]
        |> Card.view


tabCard : Model -> Html Msg
tabCard model =
    Card.config [ Card.attrs [ class "run-card" ] ]
        |> Card.headerH1 [ class "profile-header" ] [ text "Graphs" ]
        |> Card.block [ Card.blockAttrs [ class "graph-block" ] ]
            [ Card.custom <|
                div [ class "row main-row" ]
                    [ div [ class "test col-4 main-graph" ]
                        [ graphView "Distance Over Time" model.allGraphs.runDistance ]
                    , div [ class "test col-4 main-graph" ]
                        [ graphView "Mile Avg Over Time" model.allGraphs.runMileAvg ]
                    , div [ class "test col-4 main-graph" ]
                        [ graphView "Speed Over Time" model.allGraphs.runSpeedAvg ]
                    ]
            ]
        |> Card.view
