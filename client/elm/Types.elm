module Types exposing (..)

import Bootstrap.Navbar as Nav
import Dict
import Http
import Navigation exposing (Location)
import Shared.Generated exposing (User)


type Msg
    = NavMsg Nav.State
    | HandleUser User
    | HandleError Http.Error
    | LocationChange Location
    | HandleDateChange String


type Page
    = Dashboard
    | AddWorkout
    | Workouts
    | Stats
    | Profile


type alias Model =
    { navbar : Nav.State
    , user : User
    , currentPage : Page
    , queryDate : String
    , dateNames : Dict.Dict String String
    }
