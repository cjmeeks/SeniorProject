module AddWorkout.View exposing (..)

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
            [ div [ class "col-12 run" ]
                --TODO make this dynamic to each exercise same for sets
                [ exerciseView ]
            , div [ class "col-12 run" ]
                [ exerciseView ]
            , div [ class "col-12 run" ]
                [ exerciseView ]
            , div [ class "col-12 run" ]
                [ exerciseView ]
            , div [ class "row full-width" ]
                [ div [ class "col-6" ]
                    [ Button.button
                        [ Button.attrs [ class "half-screen-button add-ex-btn icon-with-text" ]
                        ]
                        [ Icon.hand_grab_o
                        , text "Add exercise"
                        ]
                    ]
                , div [ class "col-6" ]
                    [ Button.button [ Button.attrs [ class "half-screen-button add-cardio-btn icon-with-text" ] ]
                        [ Icon.heart
                        , text "Add Cardio"
                        ]
                    ]
                ]
            ]
        ]


exerciseView : Html Msg
exerciseView =
    Card.config [ Card.attrs [ class "run-card" ], Card.outlineInfo ]
        |> Card.headerH1 []
            [ div [ class "ex-header" ]
                [ text "My Exercise Info"
                , Button.button [ Button.attrs [ class "add-set" ] ] [ Icon.plus ]
                , div [ class "add-set-label" ] [ text "Add Another Set" ]
                ]
            ]
        |> Card.block []
            [ Card.custom setView
            ]
        |> Card.view


setView : Html Msg
setView =
    Card.config [ Card.attrs [ class "run-card" ], Card.outlineInfo ]
        |> Card.block []
            [ Card.text [] [ set ]
            ]
        |> Card.view



--map over list of sets


set : Html Msg
set =
    div [] [ text "set cotent", div [ class "delete-set" ] [ Button.button [ Button.attrs [ class "add-set" ] ] [ Icon.minus ] ] ]
