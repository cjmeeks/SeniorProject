module Views.Login exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Form.Input as Input
import Html exposing (..)
import Html.Attributes exposing (..)
import Types exposing (LoginMsg(..), Model, Msg(..), SignupMsg(..))


view : Model -> Html Msg
view model =
    div [ class "login-wrapper" ]
        [ div [ class "row login-input-row" ]
            [ div [ class "col-4 login-label" ] <| [ text "Username: " ] ++ [ Html.map UpdateLogin <| Input.text [ Input.large, Input.onInput LoginUserName, Input.attrs [ class "col-8 login-input" ] ] ] ]
        , div
            [ class "row login-input-row" ]
            [ div [ class "col-4 login-label" ] <| [ text "Password: " ] ++ [ Html.map UpdateLogin <| Input.password [ Input.large, Input.onInput LoginPassword, Input.attrs [ class "col-8 login-input" ] ] ] ]
        , div [ class "row login-input-row" ] <| [ Html.map UpdateLogin <| Button.button [ Button.success, Button.onClick LoginButton, Button.attrs [ class "col-12 login-input-button" ] ] [ text "Login" ] ]
        , div [ class "row login-input-row" ] <| [ Html.map UpdateSignup <| Button.button [ Button.primary, Button.onClick SignupButton, Button.attrs [ class "col-12 login-input-button" ] ] [ text "Signup" ] ]
        ]
