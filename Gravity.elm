module Gravity (force) where

import Dynamics exposing (ForceSource, Body)
import Vector exposing (Vector, minus)

force : ForceSource a
force bodies target =
  bodies
    |> List.map (forceFor target)
    |> Vector.sum

forceFor : Body a -> Body a -> Vector
forceFor target source =
  let
    direction = source.position `minus` target.position
    distance = Vector.norm direction
    magnitude = bigG * source.mass * target.mass / distance ^ 2
  in
    if source.position /= target.position then
      Vector.scale magnitude direction
    else
      Vector.zero

bigG : Float
bigG = 6.674e-11