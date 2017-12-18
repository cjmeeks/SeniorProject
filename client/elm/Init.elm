module Init exposing (..)

import Bootstrap.Navbar as Nav
import Dict
import Http
import Navigation exposing (Location)
import Ports.Ports exposing (DatePickerMsg, buildDatePicker, getCurrentDate)
import Routing exposing (parseLocation)
import Shared.Generated exposing (User, Workout, Exercise, Run, Set, Lift)
import Types exposing (Model, Msg(..), Staged, Data(..), Graphs, LoginModel)
import Bootstrap.Modal as Modal
import Bootstrap.Dropdown as Drop
import Rest exposing (getCurrentId)


initUser : User
initUser =
    { user_id = -1
    , user_first_name = "FirstName"
    , user_last_name = "LastName"
    , user_username = ""
    , user_password = ""
    , user_weight = -1
    , user_height = -1
    , user_age = -1
    , user_workouts = []
    , user_saved_workouts = []
    }


initLogin : LoginModel
initLogin =
    { username = ""
    , password = ""
    }


initWorkout : User -> Workout
initWorkout user =
    { workout_id = -1
    , workout_date = "2000-01-01"
    , workout_total_time = -1
    , workout_name = "no name"
    , workout_user_id = user.user_id
    , workout_exercises = []
    , workout_runs = []
    }


initExercise : Exercise
initExercise =
    { exercise_id = -1
    , exercise_time = -1
    , exercise_workout_id = -1
    , exercise_sets = []
    }


initRun : Run
initRun =
    { run_id = -1
    , run_distance = -1
    , run_time = -1
    , run_mile_avg = -1
    , run_speed_avg = -1
    , run_workout_id = -1
    }


initSet : Set
initSet =
    { set_lift = initLift
    , set_id = -1
    , set_weight = -1
    , set_reps = -1
    , set_exercise_id = -1
    , set_lift_id = -1
    }


initLift : Lift
initLift =
    { lift_id = -1
    , lift_name = "no name"
    }


initStaged : Staged
initStaged =
    { stagedWorkout = initWorkout initUser
    , stagedRun = initRun
    , stagedEx = initExercise
    , stagedSet = initSet
    , exId = -1
    , fetching = NotSaved
    , user = initUser
    , login = initLogin
    }


currentIdCmds =
    [ getCurrentId "w", getCurrentId "r", getCurrentId "e", getCurrentId "s", getCurrentDate "" ]


initGraphs : Graphs
initGraphs =
    { runDistance = []
    , runMileAvg = []
    , runSpeedAvg = []
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
        , staged = initStaged
        , cardioModal = Modal.hiddenState
        , exerciseModal = Modal.hiddenState
        , signupModal = Modal.hiddenState
        , setModal = Modal.hiddenState
        , addModal = Modal.hiddenState
        , setDrop = Drop.initialState
        , lifts = []
        , selectedWorkout = initWorkout initUser
        , allGraphs = initGraphs
        }
