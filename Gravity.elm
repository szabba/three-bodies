module Gravity (force) where

import Planet
import Vector exposing (Vector, minus)

force : Planet.Model -> Planet.Model -> Vector
force on from =
  let
    direction = from.position `minus` on.position
    distance = Vector.norm direction
    magnitude = bigG * from.mass * on.mass / distance ^ 2
  in
    if from.position /= on.position then
      Vector.scale magnitude direction
    else
      Vector.zero

bigG : Float
bigG = 6.674e-11