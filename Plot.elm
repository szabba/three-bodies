module Plot
  ( dataToForm ) where

import Html exposing (Html)
import Graphics.Collage as Collage
import Color

import Debug


type alias Range =
  { xMax : Float
  , xMin : Float
  , yMax : Float
  , yMin : Float
  }


type alias Data = List (Float, Float)


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