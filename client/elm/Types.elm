module Types exposing (..)

import Bootstrap.Navbar as Nav
import Dict
import Http
import Navigation exposing (Location)
import Shared.Generated exposing (User, Workout, Run, Exercise, Set, Lift)
import Bootstrap.Modal as Modal
import Bootstrap.Dropdown as Drop


type Msg
    = NavMsg Nav.State
    | HandleUser User
    | HandleError Http.Error
    | LocationChange Location
    | HandleDateChange String
    | HandleLifts (List Lift)
    | AddExercise
    | AddRun
    | CardioModalMsg Modal.State
    | ExerciseModalMsg Modal.State
    | SetModalMsg Modal.State
    | AddWorkoutModalMsg Modal.State
    | SignUpMsg Modal.State
    | SetDrop Drop.State
    | OpenCardio
    | AddExButton
    | OpenSet Int
    | CardioMsg Cardio
    | SetMsg SetFormMsg
    | SaveWorkout
    | HandleSavedUser User
    | HandleCurrentId String Int
    | ViewWorkoutClick Workout
    | OpenAdd
    | AddWorkoutModalButton
    | NameInput String
    | Cancel
    | HandleSuccess String
    | SaveUserWorkout Workout
    | HandleDelete String
    | DeleteSavedWorkout Int
    | LoginUserName String
    | LoginPassword String
    | HandleLogin User
    | HandleSignup String
    | LoginButton
    | SignupButton
    | SignupSend
    | SignupUserName String
    | SignupPassword String
    | SignupFirst String
    | SignupLast String
    | SignupWeight String
    | SignupHeight String
    | SignupAge String
    | Logout
    | AddSaved Workout
    | GetDate


type Cardio
    = AddRunButton
    | DistanceInput String
    | TimeInput String


type SetFormMsg
    = AddSetButton
    | LiftInput Lift
    | WeightInput String
    | RepInput String


type Page
    = Dashboard
    | AddWorkout
    | Workouts
    | ViewWorkout
    | Stats
    | Profile
    | SignUp
    | Login


type alias Model =
    { navbar : Nav.State
    , user : User
    , currentPage : Page
    , queryDate : String
    , dateNames : Dict.Dict String String
    , staged : Staged
    , cardioModal : Modal.State
    , exerciseModal : Modal.State
    , setModal : Modal.State
    , signupModal : Modal.State
    , addModal : Modal.State
    , setDrop : Drop.State
    , lifts : List Lift
    , selectedWorkout : Workout
    , allGraphs : Graphs
    }


type alias Graphs =
    { runDistance : List ( Float, Float )
    , runMileAvg : List ( Float, Float )
    , runSpeedAvg : List ( Float, Float )
    }


type Date
    = Date String


type Data
    = Fetching
    | Saved
    | NotSaved
    | Back


type alias LoginModel =
    { username : String
    , password : String
    }


type alias Staged =
    { stagedWorkout : Workout
    , stagedRun : Run
    , stagedEx : Exercise
    , stagedSet : Set
    , user : User
    , exId : Int
    , fetching : Data
    , login : LoginModel
    }
