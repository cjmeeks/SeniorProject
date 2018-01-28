module Update.Handle exposing (..)

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


update : HandleStuff -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        HandleSignup str ->
            ( model, Cmd.none )

        HandleLogin usr ->
            ( { model | user = usr }, Cmd.batch <| List.append currentIdCmds [ getLiftsCall, Navigation.newUrl "#dashboard" ] )

        HandleUser usr ->
            let
                staged =
                    model.staged
            in
            ( { model | user = usr, staged = { staged | stagedWorkout = initWorkout usr } }, Cmd.batch <| List.append currentIdCmds [ getLiftsCall ] )

        HandleError err ->
            let
                temp =
                    Debug.log "Error: " err
            in
            ( model, Cmd.none )

        HandleDateChange str ->
            let
                staged =
                    model.staged

                wk =
                    staged.stagedWorkout
            in
            ( { model | queryDate = str, staged = { staged | stagedWorkout = { wk | workout_date = str } } }, Cmd.none )

        HandleLifts lifts ->
            let
                temp =
                    Debug.log "workout" lifts
            in
            ( { model | lifts = lifts }, Cmd.none )

        HandleSavedUser usr ->
            let
                staged =
                    model.staged
            in
            ( { model | staged = { staged | stagedWorkout = initWorkout model.user, fetching = Saved } }, getUser usr.user_id )

        HandleCurrentId str num ->
            case str of
                "w" ->
                    let
                        staged =
                            model.staged

                        wk =
                            staged.stagedWorkout

                        run =
                            staged.stagedRun

                        ex =
                            staged.stagedEx

                        newEx =
                            { ex | exercise_workout_id = num }

                        newRun =
                            { run | run_workout_id = num }

                        newWorkout =
                            { wk | workout_id = num }

                        newStaged =
                            { staged | stagedWorkout = newWorkout, stagedEx = newEx, stagedRun = newRun }
                    in
                    ( { model | staged = newStaged }, Cmd.none )

                "e" ->
                    let
                        staged =
                            model.staged

                        ex =
                            staged.stagedEx

                        set =
                            staged.stagedSet

                        len =
                            List.length staged.stagedWorkout.workout_exercises

                        newEx =
                            { ex | exercise_id = num + len }

                        newSet =
                            { set | set_exercise_id = num + len }

                        newStaged =
                            { staged | stagedEx = newEx, stagedSet = newSet }
                    in
                    ( { model | staged = newStaged }, Cmd.none )

                "r" ->
                    let
                        staged =
                            model.staged

                        run =
                            staged.stagedRun

                        len =
                            List.length staged.stagedWorkout.workout_runs

                        newRun =
                            { run | run_id = num + len }

                        newStaged =
                            { staged | stagedRun = newRun }
                    in
                    ( { model | staged = newStaged }, Cmd.none )

                "s" ->
                    let
                        staged =
                            model.staged

                        set =
                            staged.stagedSet

                        len =
                            List.length staged.stagedEx.exercise_sets

                        newSet =
                            { set | set_id = num + len }

                        newStaged =
                            { staged | stagedSet = newSet }
                    in
                    ( { model | staged = newStaged }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        HandleSuccess str ->
            ( model, getUser model.user.user_id )

        HandleDelete str ->
            ( model, getUser model.user.user_id )
