module Signup exposing (..)

import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Form.Input as Input
import Bootstrap.Button as Button
import Bootstrap.Modal as Modal
import Bootstrap.Card as Card
import Bootstrap.Dropdown as Drop
import FontAwesome.Web as Icon
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Shared.Helper exposing (datePicker, displayMileAvg)
import Types exposing (Model, Msg(..), Cardio(..), SetFormMsg(..), Data(..))
import Shared.Generated exposing (Exercise, Workout, Set, User, Run)
import Round exposing (..)


signupModal : Model -> Html Msg
signupModal model =
    Modal.config SignUpMsg
        |> Modal.large
        |> Modal.h5 [] [ text "Signup" ]
        |> Modal.body []
            [ div []
                [ div [ class "row" ]
                    [ span [ class "col-2" ] [ text "First Name: " ]
                    , Input.text [ Input.onInput SignupFirst, Input.attrs [ class "col-10" ] ]
                    ]
                , br [] []
                , div [ class "row" ]
                    [ span [ class "col-2" ] [ text "Last Name: " ]
                    , Input.text [ Input.onInput SignupLast, Input.attrs [ class "col-10" ] ]
                    ]
                , br [] []
                , div [ class "row" ]
                    [ span [ class "col-2" ] [ text "Weight: " ]
                    , Input.number [ Input.onInput SignupWeight, Input.attrs [ class "col-10" ] ]
                    ]
                , br [] []
                , div [ class "row" ]
                    [ span [ class "col-2" ] [ text "Height:  " ]
                    , Input.number [ Input.onInput SignupHeight, Input.attrs [ class "col-10" ] ]
                    ]
                , br [] []
                , div [ class "row" ]
                    [ span [ class "col-2" ] [ text "Age:  " ]
                    , Input.number [ Input.onInput SignupAge, Input.attrs [ class "col-10" ] ]
                    ]
                , br [] []
                , div [ class "row" ]
                    [ span [ class "col-2" ] [ text "Username: " ]
                    , Input.text [ Input.onInput SignupUserName, Input.attrs [ class "col-10" ] ]
                    ]
                , br [] []
                , div [ class "row" ]
                    [ span [ class "col-2" ] [ text "Password: " ]
                    , Input.text [ Input.onInput SignupPassword, Input.attrs [ class "col-10" ] ]
                    ]
                , br [] []
                , div [ class "row" ]
                    [ Button.button [ Button.success, Button.onClick SignupSend, Button.attrs [ class "col-6" ] ] [ text "Signup" ]
                    , Button.button [ Button.danger, Button.onClick Cancel, Button.attrs [ class "col-6" ] ] [ text "Cancel" ]
                    ]
                ]
            ]
        |> Modal.view model.signupModal
