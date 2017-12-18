port module Ports.Ports exposing (..)


port buildDatePicker : DatePickerMsg -> Cmd msg


port getCurrentDate : String -> Cmd msg


port handleDateChange : (String -> msg) -> Sub msg


type alias DatePickerMsg =
    { name : String
    , dates : String
    }
