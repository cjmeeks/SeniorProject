module Update.Update exposing (..)

import Bootstrap.Modal as Modal
import Dict
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
import Routing exposing (parseLocation)
import Shared.Helper exposing (getGraphDataRunDistance, getGraphDataRunMile, getGraphDataRunSpeed, getRunStats, getTotalTimeWorkout)
import Types exposing (Cardio(..), Data(..), LoginMsg(..), Model, Msg(..), Page(..), SetFormMsg(..))


login : LoginMsg -> Model -> ( Model, Cmd Msg )
login msg model =
    case msg of
        LLoginUserName str ->
            let
                staged =
                    model.staged

                login =
                    staged.login

                newStaged =
                    { staged | login = { login | username = str } }
            in
            ( { model | staged = newStaged }, Cmd.none )

        LLoginPassword str ->
            let
                staged =
                    model.staged

                login =
                    staged.login

                newStaged =
                    { staged | login = { login | password = str } }
            in
            ( { model | staged = newStaged }, Cmd.none )

        LLoginButton ->
            let
                staged =
                    model.staged
            in
            ( { model | staged = { staged | login = initLogin } }, loginCall model.staged.login.username model.staged.login.password )

        LLogout ->
            ( { model | user = initUser, currentPage = Login }, Navigation.newUrl "#login" )
