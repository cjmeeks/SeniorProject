module Init exposing (..)

import Bootstrap.Navbar as Nav
import Dict
import Http
import Navigation exposing (Location)
import Ports.Ports exposing (DatePickerMsg, buildDatePicker)
import Routing exposing (parseLocation)
import Shared.Generated exposing (User, Workout, Exercise, Run, Set, Lift)
import Types exposing (Model, Msg(..))
import Stats.Types exposing(initStats)


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

initWorkout : Workout
initWorkout =
    { workout_id = 0
    , workout_date = ""
    , workout_total_time = 0
    , workout_type = ""
    , workout_user_id = 0
    , workout_exercises = []
    , workout_runs = []
    }

initExercise : Exercise
initExercise =
    { exercise_id =0
    , exercise_time =0
    , exercise_workout_id =0
    , exercise_sets = []
    }

initRun : Run
initRun =
    { run_id =0
    , run_distance =0
    , run_time =0
    , run_mile_avg =0
    , run_speed_avg =0
    , run_workout_id =0
    }

initSet : Set
initSet =
    { set_lift = Nothing
    , set_id = 0
    , set_weight = 0
    , set_reps = 0
    , set_exercise_id = 0
    , set_lift_id = 0
    }

initLift : Lift
initLift =
    { lift_id = 0
    , lift_name = ""
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
    , stagedWorkout = initWorkout
    }
