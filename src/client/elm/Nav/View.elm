module Nav.View exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Bootstrap.Navbar as Nav
import Types exposing (Model, Msg(..))
import FontAwesome.Web as Icon
import Color as C

view : Model -> Html Msg
view model =
    Nav.config NavMsg
        |> Nav.withAnimation
        |> Nav.brand [ href "#"] [ text "Workout Tracker"]
        |> Nav.attrs [class "nav-custom"]
        |> Nav.items
            [ Nav.itemLink [href "#", class "nav-button"] [ Icon.dashboard, text "Dashboard"]
            , Nav.itemLink [href "#"] [ Icon.plus, text "Add Workout"]
            , Nav.itemLink [href "#"] [ Icon.bars, text "Workouts"]
            , Nav.itemLink [href "#"] [ Icon.bar_chart, text "Stats"]
            , Nav.itemLink [href "#"] [ Icon.user, text "Profile"]
            ]
        |> Nav.view model.navbar

newView : Model -> Html Msg
newView model = 
    nav [ class "navbar navbar-default" ]
        [ div [ class "container-fluid" ]
            [ ul [ class "navbar-nav" ]
                [ li [ class "nav-item" ]
                    [ a [ class "nav-link", href "#" ]
                        [ text "Home"
                        ] 
                    ]
                , li [ class "nav-item" ]
                    [ a [ class "nav-link", href "#" ]
                        [ text "Link" ] 
                    ] 
                , li [ class "nav-item" ]
                    [ a [ class "nav-link", href "#" ]
                        [ text "Disabled" ] 
                    ]
                ]
            ]
        ] 
