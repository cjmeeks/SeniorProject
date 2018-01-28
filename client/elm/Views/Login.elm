module Views.Login exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Html exposing (..)
import Html.Attributes exposing (..)
import Styles.Styles as Style
import Types exposing (LoginMsg(..), Model, Msg(..), SignupMsg(..))


view : Model -> Html Msg
view model =
    div [ Style.gridAccess 2 2 ]
        [ div [ class "login-inner-grid" ]
            [ div [ Style.gridAccessSpanCol 1 5 1, class "login-header" ] [ text "Login" ]
            , div [ Style.gridAccessSpanCol 2 5 1, class "login-row" ] <| [ text "Username: " ] ++ [ Html.map UpdateLogin <| Input.text [ Input.large, Input.onInput LoginUserName, Input.attrs [ class "login-input" ] ] ]
            , div
                [ Style.gridAccessSpanCol 3 5 1, class "login-row" ]
                [ div [ class "login-label" ] <| [ text "Password: " ] ++ [ Html.map UpdateLogin <| Input.password [ Input.large, Input.onInput LoginPassword, Input.attrs [ class "login-input" ] ] ] ]
            , div [ Style.gridAccessSpanCol 4 2 1, class "login-row" ] <| [ Html.map UpdateLogin <| Button.button [ Button.success, Button.onClick LoginButton, Button.attrs [ class "col-12 login-input-button" ] ] [ text "Login" ] ]
            , div [ Style.gridAccessSpanCol 4 2 4, class "login-row" ] <| [ Html.map UpdateSignup <| Button.button [ Button.primary, Button.onClick SignupButton, Button.attrs [ class "col-12 login-input-button" ] ] [ text "Signup" ] ]
            ]
        ]
