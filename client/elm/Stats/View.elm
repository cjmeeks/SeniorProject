module Stats.View exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Tab as Tab
import Dashboard.View exposing (graphView)
import FontAwesome.Web as Icon
import Html exposing (..)
import Html.Attributes exposing (..)
import Shared.Generated exposing (User)
import Shared.Helper exposing (datePicker)
import Types exposing (Model, Msg(..))
import Stats.Types exposing (StatsModel, StatsMsg(..))


view : Model -> Html StatsMsg
view model =
    div [ class "container" ]
        [ div [ class "row" ]
            [ div [ class "col-4" ]
                [ statsView model ]
            , div [ class "col-8" ]
                [ tabCard model ]
            ]
        ]


statsView : Model -> Html StatsMsg
statsView model =
    Card.config [ Card.attrs [ class "run-card" ] ]
        |> Card.headerH1 [ class "profile-header" ] [ text "Stats" ]
        |> Card.block [ Card.blockAttrs [ class "row profile-row" ] ]
            [ Card.text [ class "col-6" ] [ label [] [ text "Weight: " ], text (" " ++ toString model.user.user_weight) ]
            , Card.text [ class "col-6" ] [ label [] [ text "Height: " ], text (" " ++ toString model.user.user_height) ]
            ]
        |> Card.block [ Card.blockAttrs [ class "row profile-row" ] ]
            [ Card.text [ class "col-6" ] [ label [] [ text "Age: " ], text (" " ++ toString model.user.user_age) ]
            , Card.text [ class "col-6" ] [ label [] [ text "# of Workouts: " ], text (" " ++ (toString <| List.length model.user.user_workouts)) ]
            ]
        |> Card.view

tabCard : Model -> Html StatsMsg
tabCard model =
    Card.config [ Card.attrs [ class "run-card" ] ]
        |> Card.headerH1 [ class "profile-header" ] [ text "Graphs" ]
        |> Card.block [] [Card.custom (tabView model.statsModel)]
        |> Card.view

tabView : StatsModel -> Html StatsMsg
tabView stats =
    Tab.config TabMsg
        |> Tab.withAnimation -- remember to wire up subscriptions when using this option
        |> Tab.right
        |> Tab.items
            [ Tab.item
                { id = "tabItem1"
                , link = Tab.link [] [ text "Tab 1" ]
                , pane = Tab.pane [] [ text "Tab 1 Content" ]
                }
            , Tab.item
                { id = "tabItem2"
                , link = Tab.link [] [ text "Tab 2" ]
                , pane = Tab.pane [] [ text "Tab 2 Content" ]
                }
            ]
        |> Tab.view stats.tabState