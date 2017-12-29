module Update.Signup exposing (..)

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
import Types exposing (Cardio(..), Data(..), Model, Msg(..), Page(..), SetFormMsg(..), SignupMsg(..))


signup : SignupMsg -> Model -> ( Model, Cmd Msg )
signup msg model =
    case msg of
        SSignupButton ->
            ( { model | signupModal = Modal.visibleState }, Cmd.none )

        SSignupSend ->
            let
                staged =
                    model.staged
            in
            ( { model | staged = { staged | user = initUser }, signupModal = Modal.hiddenState }, signupCall staged.user )

        SSignupUserName str ->
            let
                staged =
                    model.staged

                user =
                    staged.user

                newStaged =
                    { staged | user = { user | user_username = str } }
            in
            ( { model | staged = newStaged }, Cmd.none )

        SSignupPassword str ->
            let
                staged =
                    model.staged

                user =
                    staged.user

                newStaged =
                    { staged | user = { user | user_password = str } }
            in
            ( { model | staged = newStaged }, Cmd.none )

        SSignupFirst str ->
            let
                staged =
                    model.staged

                user =
                    staged.user

                newStaged =
                    { staged | user = { user | user_first_name = str } }
            in
            ( { model | staged = newStaged }, Cmd.none )

        SSignupLast str ->
            let
                staged =
                    model.staged

                user =
                    staged.user

                newStaged =
                    { staged | user = { user | user_last_name = str } }
            in
            ( { model | staged = newStaged }, Cmd.none )

        SSignupWeight str ->
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

        SSignupHeight str ->
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

        SSignupAge str ->
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
