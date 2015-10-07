module Plot
  ( view ) where

import Html exposing (Html)
import Graphics.Collage as Collage
import Color

import TimeSeries exposing (TimeSeries)

view : (Int, Int) -> TimeSeries -> Html
view dimmensions ts =
  let
    (width, height) = dimmensions
    maxValue = Maybe.withDefault 0.0 ts.maxValue
    minValue = Maybe.withDefault 0.0 ts.minValue
    totalTime = Maybe.withDefault 0.0 ts.totalTime
    scaleVertical (t, f) = (t, (f - minValue) * toFloat height / (maxValue - minValue))
    scaleHorizontal (t, f) = (t * toFloat width / totalTime, f)
    data = List.map (scaleHorizontal << scaleVertical) ts.dataPoints
    path = Collage.path data
    lineStyle = Collage.solid Color.black
    minusHalfWidth = negate <| toFloat width / 2
    minusHalfHeight = negate <| toFloat height / 2
    decentering = (minusHalfWidth, minusHalfHeight)
    plot = Collage.move decentering << Collage.traced lineStyle <| path
    collage = Collage.collage width height [ plot ]
  in
    Html.fromElement collage