module Shared.Helper exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


putTogetherDates : String -> String -> String
putTogetherDates start end =
    start ++ " TO " ++ end


datePicker : String -> Html msg
datePicker name =
    div [ class "row form-group" ]
        [ text "Date Range:"
        , input
            [ Html.Attributes.name name ]
            []
        ]
