module Rest exposing (..)

import Shared.Generated exposing (getApiUser, User)
import Http
import Types exposing (Msg(..))

rollDice : Int -> Cmd Msg
rollDice userId =
    Http.send processResult <| getApiUser (Just 2)

processResult : Result Http.Error User -> Msg
processResult result =
    case result of
        Ok us ->
            HandleUser us
        
        Err err ->
            HandleError err