module Main exposing (..)

import Bootstrap.Dropdown as Drop
import Bootstrap.Navbar as Nav
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
import Navigation exposing (Location)
import Ports.Ports exposing (DatePickerMsg, buildDatePicker, getCurrentDate, handleDateChange)
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
import Types exposing (Cardio(..), Data(..), HandleStuff(..), Model, Msg(..), Page(..), SetFormMsg(..))
import Update.Update as U
import Views.Top exposing (view)


main : Program Never Model Msg
main =
    Navigation.program LocationChange
        { init = init
        , view = view
        , update = U.fakeAuth
        , subscriptions = subscriptions
        }


init : Location -> ( Model, Cmd Msg )
init loc =
    let
        initModel =
            initialModel loc
    in
    ( initModel
    , Cmd.batch
        [ getLiftsCall ]
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Nav.subscriptions model.navbar NavMsg
        , Sub.batch [ Sub.map UpdateHandle <| handleDateChange HandleDateChange ]
        , Drop.subscriptions model.setDrop SetDrop
        ]
