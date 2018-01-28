module Update.Cardio exposing (..)

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
import Shared.Helper exposing (..)
import Types exposing (Cardio(..), Data(..), HandleStuff(..), Model, Msg(..), Page(..), SetFormMsg(..))


update : Cardio -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddRunButton ->
            let
                staged =
                    model.staged

                wk =
                    staged.stagedWorkout

                run =
                    getRunStats staged.stagedRun

                newWorkout =
                    { wk | workout_runs = List.append wk.workout_runs [ run ] }
            in
            ( { model | staged = { staged | stagedWorkout = newWorkout }, cardioModal = Modal.hiddenState }, Cmd.batch currentIdCmds )

        DistanceInput str ->
            let
                staged =
                    model.staged

                run =
                    staged.stagedRun

                convert =
                    case String.toFloat str of
                        Ok a ->
                            a

                        Err str ->
                            let
                                temp =
                                    Debug.log "Error conversion:" str
                            in
                            -1

                newRun =
                    { run | run_distance = convert }
            in
            ( { model | staged = { staged | stagedRun = newRun } }, Cmd.none )

        TimeInput str ->
            let
                staged =
                    model.staged

                run =
                    staged.stagedRun

                convert =
                    case String.toFloat str of
                        Ok a ->
                            a

                        Err str ->
                            let
                                temp =
                                    Debug.log "Error conversion:" str
                            in
                            -1

                newRun =
                    { run | run_time = convert }
            in
            ( { model | staged = { staged | stagedRun = newRun } }, Cmd.none )

        CCancel ->
            ( { model | addModal = Modal.hiddenState, setModal = Modal.hiddenState, cardioModal = Modal.hiddenState, signupModal = Modal.hiddenState }, Cmd.none )
