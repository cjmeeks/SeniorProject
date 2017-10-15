module Init exposing (..)
import Http
import Bootstrap.Navbar as Nav
import Shared.Generated exposing (User)
import Navigation exposing (Location)
import Routing exposing (parseLocation)
import Types exposing (Msg(..), Model)


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

initialModel : Location -> (Model, Cmd Msg)
initialModel loc =
    let
        (navbar, navCmd) = Nav.initialState NavMsg
    in
        ({ navbar = navbar
         , user = initUser
         , currentPage = parseLocation loc 
         }
        , navCmd)