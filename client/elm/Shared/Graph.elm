module Shared.Graph exposing (..)

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Plot
import Shared.Helper exposing (getGraphDataRunDistance, getGraphDataRunMile, getGraphDataRunSpeed)
import Svg exposing (Svg)
import Svg.Attributes as Sa
import Types exposing (Model, Msg)


--model functions


type alias Day =
    { index : Int
    , wins : Int
    }



--viewhelper functions
--view


mainView : List ( Float, Float ) -> Html a
mainView list =
    div [] [ plot list ]


plot : List ( Float, Float ) -> Html a
plot data =
    Plot.viewSeries
        [ distanceSeries (List.map (\( x, y ) -> Plot.circle x y)) ]
        data


distanceSeries : (data -> List (Plot.DataPoint msg)) -> Plot.Series data msg
distanceSeries toDataPoints =
    { axis = distanceAxis
    , interpolation = Plot.Linear Nothing [ Sa.stroke "black" ]
    , toDataPoints = toDataPoints
    }


distanceAxis : Plot.Axis
distanceAxis =
    Plot.customAxis <|
        \summary ->
            { position = Plot.closestToZero
            , axisLine = Just (Plot.simpleLine summary)
            , ticks = List.map Plot.simpleTick (Plot.interval 0 1 summary |> Plot.remove 0)
            , labels = List.map distanceLabel (Plot.decentPositions summary |> Plot.remove 0)
            , flipAnchor = False
            }


distanceLabel : Float -> Plot.LabelCustomizations
distanceLabel x =
    { view = viewLabel [] (toString x)
    , position = x
    }


viewLabel : List (Svg.Attribute msg) -> String -> Svg msg
viewLabel attributes string =
    Svg.text_ attributes [ Svg.tspan [] [ text string ] ]
