module Update.Modal exposing (..)

import Bootstrap.Modal as Modal
import Init
    exposing
        ( currentIdCmds
        , initExercise
        , initLift
        , initLogin
        , initRun
        , initSet
        , initUser
        , initWorkout
        , initialModel
        )
import Rest
    exposing
        ( addWorkout
        , deleteSavedWorkout
        , getCurrentId
        , getLiftsCall
        , getUser
        , loginCall
        , saveWorkout
        , signupCall
        )
import Types exposing (Cardio(..), Data(..), Model, Msg(..), Page(..), SetFormMsg(..), ModalMsgs(..))


updateModals ModalMsgs -> Model -> ( Model, Cmd Msg )
updateModals msg model =
  case msg of
    = MCardioModalMsg Modal.State
    | MExerciseModalMsg Modal.State
    | MSetModalMsg Modal.State
    | MAddWorkoutModalMsg Modal.State
    | MSignUpMsg Modal.State