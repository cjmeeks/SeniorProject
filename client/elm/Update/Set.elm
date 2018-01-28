module Update.Set exposing (..)

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
import Types exposing (Cardio(..), Data(..), HandleStuff(..), Model, Msg(..), Page(..), SetFormMsg(..))


update : SetFormMsg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddSetButton ->
            let
                staged =
                    model.staged

                wk =
                    staged.stagedWorkout

                exs =
                    Dict.fromList <| List.map (\x -> ( x.exercise_id, x )) wk.workout_exercises

                currentEx =
                    case Dict.get staged.exId exs of
                        Just a ->
                            a

                        Nothing ->
                            initExercise

                set =
                    staged.stagedSet

                newEx =
                    { currentEx | exercise_sets = List.append currentEx.exercise_sets [ { set | set_exercise_id = currentEx.exercise_id } ] }

                newWorkout =
                    if currentEx.exercise_id == initExercise.exercise_id then
                        wk
                    else
                        { wk | workout_exercises = List.map Tuple.second <| Dict.toList <| Dict.insert staged.exId newEx exs }

                newStaged =
                    { staged | stagedSet = initSet, stagedWorkout = newWorkout, exId = -1 }
            in
            ( { model | setModal = Modal.hiddenState, staged = newStaged }, Cmd.batch currentIdCmds )

        LiftInput li ->
            let
                staged =
                    model.staged

                set =
                    staged.stagedSet

                newStaged =
                    { staged | stagedSet = { set | set_lift = li, set_lift_id = li.lift_id } }

                temp =
                    Debug.log "lid" newStaged.stagedSet.set_lift_id
            in
            ( { model | staged = newStaged }, Cmd.none )

        WeightInput str ->
            let
                staged =
                    model.staged

                set =
                    staged.stagedSet

                convert =
                    case String.toInt str of
                        Ok a ->
                            a

                        Err str ->
                            let
                                temp =
                                    Debug.log "Error conversion:" str
                            in
                            -1

                newSet =
                    { set | set_weight = convert }

                newStaged =
                    { staged | stagedSet = newSet }
            in
            ( { model | staged = newStaged }, Cmd.none )

        RepInput str ->
            let
                staged =
                    model.staged

                set =
                    staged.stagedSet

                convert =
                    case String.toInt str of
                        Ok a ->
                            a

                        Err str ->
                            let
                                temp =
                                    Debug.log "Error conversion:" str
                            in
                            -1

                newSet =
                    { set | set_reps = convert }

                newStaged =
                    { staged | stagedSet = newSet }
            in
            ( { model | staged = newStaged }, Cmd.none )

        SetCancel ->
            ( { model | addModal = Modal.hiddenState, setModal = Modal.hiddenState, cardioModal = Modal.hiddenState, signupModal = Modal.hiddenState }, Cmd.none )
