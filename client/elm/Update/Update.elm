module Update.Update exposing (fakeAuth)

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
import Types exposing (Cardio(..), Data(..), Model, Msg(..), Page(..), SetFormMsg(..))
import Update.Handle as UHandle
import Update.Login as ULogin
import Update.Signup as USignup
import Update.Cardio as UCardio
import Update.Set as USet


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateLogin ya ->
            ULogin.login ya model

        UpdateSignup ya ->
            USignup.signup ya model

        UpdateHandle ya ->
            UHandle.update ya model

        NavMsg state ->
            ( { model | navbar = state }, Cmd.none )

        LocationChange loc ->
            ( { model | currentPage = parseLocation loc }, Cmd.none )

        AddExercise ->
            ( model, Cmd.none )

        AddRun ->
            ( model, Cmd.none )

        CardioModalMsg state ->
            ( { model | cardioModal = state }, Cmd.none )

        ExerciseModalMsg state ->
            ( { model | exerciseModal = state }, Cmd.none )

        AddWorkoutModalMsg state ->
            ( { model | addModal = state }, Cmd.none )

        SetModalMsg state ->
            ( { model | setModal = state }, Cmd.none )

        SignUpMsg state ->
            ( { model | signupModal = state }, Cmd.none )

        SetDrop state ->
            ( { model | setDrop = state }, Cmd.none )

        OpenCardio ->
            let
                staged =
                    model.staged
            in
            ( { model | cardioModal = Modal.visibleState, staged = { staged | fetching = NotSaved } }, Cmd.none )

        OpenSet id ->
            let
                staged =
                    model.staged

                newStaged =
                    { staged | exId = id, fetching = NotSaved }
            in
            ( { model | setModal = Modal.visibleState, staged = newStaged }, Cmd.none )

        CardioMsg cMsg ->
            UCardio.update cMsg model

        AddExButton ->
            let
                staged =
                    model.staged

                wk =
                    staged.stagedWorkout

                ex =
                    staged.stagedEx

                newWorkout =
                    { wk | workout_exercises = List.append wk.workout_exercises [ ex ] }

                newStaged =
                    { staged | stagedEx = initExercise, stagedWorkout = newWorkout, fetching = NotSaved }
            in
            ( { model | exerciseModal = Modal.hiddenState, staged = newStaged }, Cmd.batch currentIdCmds )

        SetMsg m ->
            USet.update m model

        SaveWorkout ->
            let
                staged =
                    model.staged
            in
            ( { model | staged = { staged | stagedWorkout = initWorkout model.user, fetching = Fetching } }, Cmd.batch <| List.append currentIdCmds [ addWorkout model.user.user_id (getTotalTimeWorkout model.staged.stagedWorkout) ] )

        SaveUserWorkout wk ->
            ( model, saveWorkout model.user.user_id wk )

        DeleteSavedWorkout id ->
            ( model, deleteSavedWorkout id )

        ViewWorkoutClick wk ->
            ( { model | selectedWorkout = wk, currentPage = ViewWorkout }, Navigation.newUrl "#viewW" )

        AddWorkoutModalButton ->
            ( { model | addModal = Modal.hiddenState }, Navigation.newUrl "#addworkout" )

        Cancel ->
            ( { model | addModal = Modal.hiddenState, setModal = Modal.hiddenState, cardioModal = Modal.hiddenState, signupModal = Modal.hiddenState }, Cmd.none )

        NameInput str ->
            let
                staged =
                    model.staged

                wk =
                    staged.stagedWorkout
            in
            ( { model | staged = { staged | stagedWorkout = { wk | workout_name = str } } }, Cmd.none )

        OpenAdd ->
            ( { model | addModal = Modal.visibleState }, Cmd.none )

        AddSaved wk ->
            ( model, addWorkout model.user.user_id { wk | workout_date = model.queryDate } )

        GetDate ->
            ( model, getCurrentDate "" )


fakeAuth : Msg -> Model -> ( Model, Cmd Msg )
fakeAuth msg model =
    if String.isEmpty model.user.user_username && String.isEmpty model.user.user_password && (model.currentPage /= Login) then
        ( { model | currentPage = Login }, Cmd.batch [ Navigation.newUrl "#login" ] )
    else
        updateGraphs msg model


updateGraphs : Msg -> Model -> ( Model, Cmd Msg )
updateGraphs msg model =
    let
        graphs =
            model.allGraphs

        newGraphs =
            { graphs
                | runDistance = getGraphDataRunDistance model.user.user_workouts
                , runMileAvg = getGraphDataRunMile model.user.user_workouts
                , runSpeedAvg = getGraphDataRunSpeed model.user.user_workouts
            }

        ( newModel, cmds ) =
            update msg { model | allGraphs = newGraphs }

        temp =
            Debug.log "graphs" <| List.length model.lifts
    in
    ( newModel, cmds )
