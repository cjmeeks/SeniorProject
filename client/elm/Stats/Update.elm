module Stats.Update exposing (..)

import Stats.Types exposing (StatsMsg(..), StatsModel)
import Types exposing (Model)



update : StatsMsg -> Model -> (Model, Cmd StatsMsg)
update msg model =
    let
        statsModel = model.statsModel
        (updatedStats, cmd) = 
            case msg of 
                TabMsg state ->
                    ( { statsModel | tabState = state }
                    , Cmd.none
                    )
    in
        ({model | statsModel = updatedStats}, cmd)