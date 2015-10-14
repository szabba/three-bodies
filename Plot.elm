module Plot
  ( view ) where

import Html exposing (Html)
import Graphics.Collage as Collage
import Color

import TimeSeries exposing (TimeSeries)

import Debug


type alias Range =
  { xMax : Float
  , xMin : Float
  , yMax : Float
  , yMin : Float
  }


type alias Data = List (Float, Float)


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
    plotForm = dataToForm lineStyle dimmensions range data
    collage = Collage.collage width height [ plotForm ]
  in
    Html.fromElement collage


dataToForm : Collage.LineStyle -> (Int, Int) -> Range -> Data -> Collage.Form
dataToForm lineStyle dimmensions range data =
  let
    (width, height) = dimmensions
    scaleBoth (x, y) =
      ( (x - range.xMin) * toFloat width / (range.xMax - range.xMin)
      , (y - range.yMin) * toFloat height / (range.yMax - range.yMin)
      )
    scaledData = List.map scaleBoth data
    path = Collage.path << List.map scaleBoth <| data
    minusHalfHeight = negate <| toFloat height / 2
    minusHalfWidth = negate <| toFloat width / 2
    decentering = (minusHalfWidth, minusHalfHeight)
  in
    Collage.move decentering << Collage.traced lineStyle <| path