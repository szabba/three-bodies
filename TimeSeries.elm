module TimeSeries
  ( TimeSeries, empty, append , view ) where

import Html exposing (Html)

import Plot
import Graphics.Collage as Collage
import Color

import Maybe


{-| A TimeSeries stores historical data describing how a scalar value has
changed throughout time.
-}
type alias TimeSeries =
  { totalTime : Maybe Float
  , maxValue : Maybe Float
  , minValue : Maybe Float
  , dataPoints : List (Float, Float)
  }


{-| A time series that doesn't contain any data yet.
-}
empty : TimeSeries
empty =
  { totalTime = Nothing
  , minValue = Nothing
  , maxValue = Nothing
  , dataPoints = []
  }

{-| Appends a data point to a time series. -}
append : TimeSeries -> { dt : Float, value : Float } -> TimeSeries
append timeSeries {dt, value} =
  let
    {totalTime, minValue, maxValue, dataPoints} = timeSeries
    newTotalTime = updateMaybe dt (+) totalTime
    newDataPoint = (newTotalTime, value)
  in
    { timeSeries
    | totalTime <- Maybe.Just newTotalTime
    , minValue <- Maybe.Just <| updateMaybe value min minValue
    , maxValue <- Maybe.Just <| updateMaybe value max maxValue
    , dataPoints <- dataPoints ++ [newDataPoint]
    }


updateMaybe : a -> (a -> a -> a) -> Maybe a -> a
updateMaybe new mappend old =
  old
    |> Maybe.map (mappend new)
    |> Maybe.withDefault new


{-| Turns a TimeSeries into a plot with the given pixel dimmensions.
-}
view : (Int, Int) -> TimeSeries -> Html
view dimmensions ts =
  let
    maxValue = Maybe.withDefault 0.0 ts.maxValue
    minValue = Maybe.withDefault 0.0 ts.minValue
    totalTime = Maybe.withDefault 0.0 ts.totalTime
    (width, height) = dimmensions
    range = { xMin = 0.0, xMax = totalTime, yMin = minValue, yMax = maxValue }
    data = ts.dataPoints
    lineStyle = Collage.solid Color.black
    plotForm = Plot.dataToForm lineStyle dimmensions range data
    collage = Collage.collage width height [ plotForm ]
  in
    Html.fromElement collage
