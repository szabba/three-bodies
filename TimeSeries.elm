module TimeSeries
  ( TimeSeries, empty, append , view, viewMultiple ) where

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
    (width, height) = dimmensions
    lineStyle = Collage.solid Color.black
    plotForm = Plot.dataToForm lineStyle dimmensions (toRange ts) ts.dataPoints
    collage = Collage.collage width height [ plotForm ]
  in
    Html.fromElement collage


viewMultiple : (Int, Int) -> List (Collage.LineStyle, TimeSeries) -> Html
viewMultiple dimmensions styledTSs =
  let
    (width, height) = dimmensions
    range =
      List.map (toRange << snd) styledTSs
        |> List.foldl Plot.rangeMax Plot.emptyRange
    plotForms =
      styledTSs
        |> List.map
          (\(lineStyle, ts) -> Plot.dataToForm lineStyle dimmensions range ts.dataPoints)
  in
    Html.fromElement <| Collage.collage width height plotForms


toForm : (Int, Int) -> Plot.Range -> Collage.LineStyle -> TimeSeries -> Collage.Form
toForm dimmensions range lineStyle ts =
  Plot.dataToForm lineStyle dimmensions (toRange ts) ts.dataPoints


toRange : TimeSeries -> Plot.Range
toRange ts =
  let
    maxValue = Maybe.withDefault 0.0 ts.maxValue
    minValue = Maybe.withDefault 0.0 ts.minValue
    totalTime = Maybe.withDefault 0.0 ts.totalTime
  in
    { xMin = 0.0, xMax = totalTime, yMin = minValue, yMax = maxValue }