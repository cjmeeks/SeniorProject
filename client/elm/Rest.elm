module Rest exposing (..)

import Http
import Shared.Generated exposing (Exercise, Lift, Run, Set, User, Workout, getApiUser)
import Types exposing (Msg(..))


getUser : Int -> Cmd Msg
getUser userId =
    Http.send processUser <| getApiUser (Just 1)


processUser : Result Http.Error User -> Msg
processUser result =
    case result of
        Ok us ->
            HandleUser us

        Err err ->
            HandleError err
