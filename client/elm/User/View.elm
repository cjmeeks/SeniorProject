module User.View exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Dashboard.View exposing (graphView)
import FontAwesome.Web as Icon
import Html exposing (..)
import Html.Attributes exposing (..)
import Shared.Generated exposing (User)
import Shared.Helper exposing (datePicker)
import Types exposing (Model, Msg(..))


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ div [ class "" ]
            [ div [ class "col-11 user-profile" ] [ userView model.user ]
            , div [ class "col-11 user-profile" ] [ profileStatsView model.user ]
            ]
        , div [ class "settings-header" ] [ h1 [] [ text "Settings" ] ]
        , div [ class "row main-row" ]
            [ div [ class "col-4" ]
                [ changeGraphView ]
            , div [ class "col-4" ]
                [ changeGraphView ]
            , div [ class "col-4" ]
                [ changeGraphView ]
            ]
        ]


userView : User -> Html Msg
userView user =
    Card.config [ Card.attrs [ class "run-card" ] ]
        |> Card.headerH1 [ class "user-header" ]
            [ div [ class "profile-header" ]
                [ text (user.user_first_name ++ " " ++ user.user_last_name ++ " (" ++ user.user_username ++ ")")
                , div [ class "edit-profile" ]
                    [ Button.button [ Button.success ] [ text "Edit" ] ]
                ]
            ]
        |> Card.block [ Card.blockAttrs [ class "row profile-row" ] ]
            [ Card.text [ class "col-6" ] [ label [] [ text "Weight: " ], text (" " ++ toString user.user_weight) ]
            , Card.text [ class "col-6" ] [ label [] [ text "Height: " ], text (" " ++ toString user.user_height) ]
            ]
        |> Card.block [ Card.blockAttrs [ class "row profile-row" ] ]
            [ Card.text [ class "col-6" ] [ label [] [ text "Age: " ], text (" " ++ toString user.user_age) ]
            , Card.text [ class "col-6" ] [ label [] [ text "# of Workouts: " ], text (" " ++ (toString <| List.length user.user_workouts)) ]
            ]
        |> Card.view


profileStatsView : User -> Html Msg
profileStatsView user =
    Card.config [ Card.attrs [ class "run-card" ] ]
        |> Card.headerH1 [ class "user-header" ]
            [ div [ class "profile-header" ]
                [ text "Profile Stats"
                ]
            ]
        |> Card.block [ Card.blockAttrs [ class "row profile-row" ] ]
            [ Card.text [ class "col-6" ] [ label [] [ text "General Stat: " ], text (" " ++ toString user.user_weight) ]
            , Card.text [ class "col-6" ] [ label [] [ text "General Stat: " ], text (" " ++ toString user.user_height) ]
            ]
        |> Card.block [ Card.blockAttrs [ class "row profile-row" ] ]
            [ Card.text [ class "col-6" ] [ label [] [ text "General Stat: " ], text (" " ++ toString user.user_age) ]
            , Card.text [ class "col-6" ] [ label [] [ text "General Stat: " ], text (" " ++ (toString <| List.length user.user_workouts)) ]
            ]
        |> Card.view


changeGraphView : Html Msg
changeGraphView =
    Card.config [ Card.outlineInfo ]
        |> Card.headerH1 [ class "profile-header edit-graph-header" ]
            [ text "Graph"
            , div [ class "edit-profile" ]
                [ Button.button [ Button.success ] [ text "Edit" ]
                ]
            ]
        |> Card.block []
            [ Card.titleH1 [] [ text "Block title" ]
            , Card.text [] [ text "Some block content" ]
            ]
        |> Card.view
