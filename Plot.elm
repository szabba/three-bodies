module Plot
  ( Plot, view ) where

import Graphics.Collage as Collage


type alias Plot = List (Float, Float)


view : Collage.LineStyle -> (Int, Int) -> Plot -> Collage.Form
view lineStyle dimmensions plotData =
  plotData
    |> Collage.path
    |> Collage.traced lineStyle