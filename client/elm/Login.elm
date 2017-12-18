module Login exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Form.Input as Input
import FontAwesome.Web as Icon
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Shared.Generated exposing (Workout)
import Shared.Helper exposing (datePicker)
import Types exposing (Model, Msg(..))
import Signup


view : Model -> Html Msg
view model =
    div [ class "login-wrapper" ]
        [ div [ class "row login-input-row" ]
            [ div [ class "col-4 login-label" ] [ text "Username: " ]
            , Input.text [ Input.large, Input.onInput LoginUserName, Input.attrs [ class "col-8 login-input" ] ]
            ]
        , div
            [ class "row login-input-row" ]
            [ div [ class "col-4 login-label" ] [ text "Password: " ]
            , Input.password [ Input.large, Input.onInput LoginPassword, Input.attrs [ class "col-8 login-input" ] ]
            ]
        , div [ class "row login-input-row" ]
            [ Button.button [ Button.success, Button.onClick LoginButton, Button.attrs [ class "col-12 login-input-button" ] ] [ text "Login" ] ]
        , div [ class "row login-input-row" ]
            [ Button.button [ Button.primary, Button.onClick SignupButton, Button.attrs [ class "col-12 login-input-button" ] ] [ text "Signup" ]
            ]
        ]
