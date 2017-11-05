module Stats.Types exposing (..)

import Bootstrap.Tab as Tab

type alias StatsModel =
    { tabState : Tab.State }


initStats : StatsModel
initStats =
    { tabState = Tab.initialState}


type StatsMsg
    = TabMsg Tab.State