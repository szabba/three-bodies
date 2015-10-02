module Planet.Scale
  (meter) where

import Planet exposing (Planet, System)
import Vector exposing (Vector)
import Scale exposing (Meter)

meter : Meter System
meter system =
  let
    xs = List.map (reaches .x >> ceiling) system.bodies
    ys = List.map (reaches .y >> ceiling) system.bodies

    dimmensionAlongAxisWithCoordinates coordinates =
        coordinates
          |> absMaximum 0
          |> \half -> 2 * half

    width = xs |> dimmensionAlongAxisWithCoordinates
    height = ys |> dimmensionAlongAxisWithCoordinates
  in
   (width, height)

reaches : (Vector -> Float) -> Planet -> Float
reaches along planet =
  let
    {position, radius} = planet
    coordinate = along position
    sign = if coordinate < 0 then -1 else 1
  in
    sign * radius + coordinate


absMaximum : comparable -> List comparable -> comparable
absMaximum zero coll =
  coll
    |> List.map abs
    |> List.maximum
    |> Maybe.withDefault zero