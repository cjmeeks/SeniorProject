module Routing exposing (..)

import Navigation exposing (Location)
import Types exposing (Page(..))
import UrlParser exposing (..)


matchers : Parser (Page -> a) a
matchers =
    oneOf
        [ map Login top
        , map AddWorkout (s "addworkout")
        , map Workouts (s "workouts")
        , map Stats (s "stats")
        , map Profile (s "profile")
        , map ViewWorkout (s "viewW")
        , map Login (s "login")
        , map SignUp (s "singup")
        ]


parseLocation : Location -> Page
parseLocation location =
    case (parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            Dashboard
