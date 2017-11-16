module Init exposing (..)

import Bootstrap.Navbar as Nav
import Dict
import Http
import Navigation exposing (Location)
import Ports.Ports exposing (DatePickerMsg, buildDatePicker)
import Routing exposing (parseLocation)
import Shared.Generated exposing (User)
import Types exposing (Model, Msg(..))
import Stats.Types exposing (initStats)


initUser : User
initUser =
    { user_id = 0
    , user_first_name = "FirstName"
    , user_last_name = "LastName"
    , user_username = "UserName"
    , user_weight = 0
    , user_height = 0
    , user_age = 0
    , user_workouts = []
    }


initialModel : Location -> Model
initialModel loc =
    let
        ( navbar, navCmd ) =
            Nav.initialState NavMsg
    in
        { navbar = navbar
        , user = initUser
        , currentPage = parseLocation loc
        , queryDate = ""
        , dateNames = Dict.fromList <| [ ( "workoutDateRange", "wk-date-range" ) ]
        , statsModel = initStats
        , currentWorkout = initWorkout
        }
