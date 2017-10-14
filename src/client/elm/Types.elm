module Types exposing (..)
import Http
import Bootstrap.Navbar as Nav
import Shared.Generated exposing (User)

type Msg
  = NavMsg Nav.State
  | HandleUser User
  | HandleError Http.Error

type alias Model =
    { navbar : Nav.State
    , user : User
    }
initUser : User
initUser =
    { user_id = 0
    , user_first_name = "String"
    , user_last_name = "String"
    , user_username = "String"
    , user_weight = 0
    , user_height = 0
    , user_age = 0
    , user_workouts = []
    }
initialModel : (Model, Cmd Msg)
initialModel =
    let
        (navbar, navCmd) = Nav.initialState NavMsg
    in
        ({ navbar = navbar, user = initUser }, navCmd)
