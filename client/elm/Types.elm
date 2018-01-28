module Types exposing (..)

import Bootstrap.Dropdown as Drop
import Bootstrap.Modal as Modal
import Bootstrap.Navbar as Nav
import Dict
import Http
import Navigation exposing (Location)
import Shared.Generated exposing (Exercise, Lift, Run, Set, User, Workout)


type Msg
    = NavMsg Nav.State
    | AddExButton
    | AddRun
    | AddExercise
    | AddWorkoutModalMsg Modal.State --
    | AddWorkoutModalButton
    | AddSaved Workout
    | CardioModalMsg Modal.State --
    | CardioMsg Cardio
    | Cancel
    | DeleteSavedWorkout Int
    | ExerciseModalMsg Modal.State --
    | GetDate
    | LocationChange Location
    | NameInput String
    | OpenCardio
    | OpenSet Int
    | OpenAdd
    | SetModalMsg Modal.State --
    | SignUpMsg Modal.State --
    | SetDrop Drop.State --
    | SetMsg SetFormMsg
    | SaveWorkout
    | SaveUserWorkout Workout
    | ViewWorkoutClick Workout
    | UpdateLogin LoginMsg
    | UpdateSignup SignupMsg
    | UpdateHandle HandleStuff


type LoginMsg
    = LoginUserName String
    | LoginPassword String
    | LoginButton
    | Logout


type SignupMsg
    = SignupButton
    | SignupSend
    | SignupUserName String
    | SignupPassword String
    | SignupFirst String
    | SignupLast String
    | SignupWeight String
    | SignupHeight String
    | SignupAge String
    | SCancel


type HandleStuff
    = HandleSignup String
    | HandleLogin User
    | HandleUser User
    | HandleError Http.Error
    | HandleDateChange String
    | HandleLifts (List Lift)
    | HandleSavedUser User
    | HandleCurrentId String Int
    | HandleSuccess String
    | HandleDelete String


type Cardio
    = AddRunButton
    | DistanceInput String
    | TimeInput String
    | CCancel


type SetFormMsg
    = AddSetButton
    | LiftInput Lift
    | WeightInput String
    | RepInput String
    | SetCancel


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
