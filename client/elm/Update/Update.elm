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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateLogin ya ->
            ( model, Cmd.none )

        UpdateSignup ya ->
            ( model, Cmd.none )

        UpdateModals ya ->
            ( model, Cmd.none )

        UpdateHandle ya ->
            ( model, Cmd.none )

        HandleSuccess str ->
            ( model, getUser model.user.user_id )

        NavMsg state ->
            ( { model | navbar = state }, Cmd.none )

        HandleUser us ->
            let
                staged =
                    model.staged
            in
            ( { model | user = us, staged = { staged | stagedWorkout = initWorkout us } }, Cmd.batch <| List.append currentIdCmds [ getLiftsCall ] )

        HandleError err ->
            let
                temp =
                    Debug.log "Error: " err
            in
            ( model, Cmd.none )

        HandleDateChange date ->
            let
                staged =
                    model.staged

                wk =
                    staged.stagedWorkout
            in
            ( { model | queryDate = date, staged = { staged | stagedWorkout = { wk | workout_date = date } } }, Cmd.none )

        HandleLifts lifts ->
            let
                temp =
                    Debug.log "workout" lifts
            in
            ( { model | lifts = lifts }, Cmd.none )

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
            case cMsg of
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
            case m of
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

        SaveWorkout ->
            let
                staged =
                    model.staged
            in
            ( { model | staged = { staged | stagedWorkout = initWorkout model.user, fetching = Fetching } }, Cmd.batch <| List.append currentIdCmds [ addWorkout model.user.user_id (getTotalTimeWorkout model.staged.stagedWorkout) ] )

        SaveUserWorkout wk ->
            ( model, saveWorkout model.user.user_id wk )

        HandleSavedUser user ->
            let
                staged =
                    model.staged
            in
            ( { model | staged = { staged | stagedWorkout = initWorkout model.user, fetching = Saved } }, getUser user.user_id )

        HandleDelete str ->
            ( model, getUser model.user.user_id )

        DeleteSavedWorkout id ->
            ( model, deleteSavedWorkout id )

        HandleCurrentId what id ->
            case what of
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
                            { ex | exercise_workout_id = id }

                        newRun =
                            { run | run_workout_id = id }

                        newWorkout =
                            { wk | workout_id = id }

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
                            { ex | exercise_id = id + len }

                        newSet =
                            { set | set_exercise_id = id + len }

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
                            { run | run_id = id + len }

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
                            { set | set_id = id + len }

                        newStaged =
                            { staged | stagedSet = newSet }
                    in
                    ( { model | staged = newStaged }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

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

        LoginUserName str ->
            let
                staged =
                    model.staged

                login =
                    staged.login

                newStaged =
                    { staged | login = { login | username = str } }
            in
            ( { model | staged = newStaged }, Cmd.none )

        LoginPassword str ->
            let
                staged =
                    model.staged

                login =
                    staged.login

                newStaged =
                    { staged | login = { login | password = str } }
            in
            ( { model | staged = newStaged }, Cmd.none )

        LoginButton ->
            let
                staged =
                    model.staged
            in
            ( { model | staged = { staged | login = initLogin } }, loginCall model.staged.login.username model.staged.login.password )

        HandleLogin user ->
            ( { model | user = user }, Cmd.batch <| List.append currentIdCmds [ getLiftsCall, Navigation.newUrl "#dashboard" ] )

        HandleSignup str ->
            ( model, Cmd.none )

        SignupButton ->
            ( { model | signupModal = Modal.visibleState }, Cmd.none )

        SignupUserName str ->
            let
                staged =
                    model.staged

                user =
                    staged.user

                newStaged =
                    { staged | user = { user | user_username = str } }
            in
            ( { model | staged = newStaged }, Cmd.none )

        SignupPassword str ->
            let
                staged =
                    model.staged

                user =
                    staged.user

                newStaged =
                    { staged | user = { user | user_password = str } }
            in
            ( { model | staged = newStaged }, Cmd.none )

        SignupFirst str ->
            let
                staged =
                    model.staged

                user =
                    staged.user

                newStaged =
                    { staged | user = { user | user_first_name = str } }
            in
            ( { model | staged = newStaged }, Cmd.none )

        SignupLast str ->
            let
                staged =
                    model.staged

                user =
                    staged.user

                newStaged =
                    { staged | user = { user | user_last_name = str } }
            in
            ( { model | staged = newStaged }, Cmd.none )

        SignupWeight str ->
            let
                staged =
                    model.staged

                user =
                    staged.user

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

                newStaged =
                    { staged | user = { user | user_weight = convert } }
            in
            ( { model | staged = newStaged }, Cmd.none )

        SignupHeight str ->
            let
                staged =
                    model.staged

                user =
                    staged.user

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

                newStaged =
                    { staged | user = { user | user_height = convert } }
            in
            ( { model | staged = newStaged }, Cmd.none )

        SignupAge str ->
            let
                staged =
                    model.staged

                user =
                    staged.user

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

                newStaged =
                    { staged | user = { user | user_age = convert } }
            in
            ( { model | staged = newStaged }, Cmd.none )

        SignupSend ->
            let
                staged =
                    model.staged
            in
            ( { model | staged = { staged | user = initUser }, signupModal = Modal.hiddenState }, signupCall staged.user )

        Logout ->
            ( { model | user = initUser, currentPage = Login }, Navigation.newUrl "#login" )

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
