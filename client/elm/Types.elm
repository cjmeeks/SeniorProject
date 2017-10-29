module Types exposing (..)
import Http
import Bootstrap.Navbar as Nav
import Shared.Generated exposing (User)
import Navigation exposing (Location)

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
    }


