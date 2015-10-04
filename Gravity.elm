module Gravity
  ( interaction, force, potential ) where

import Dynamics exposing (Interaction, Body)
import Vector exposing (Vector, minus)


interaction : Interaction a
interaction {source, target} =
  { force = force source target
  , potential = potential source target
  }


force : Body a -> Body a -> Vector
force source target =
  let
    direction = source.position `minus` target.position
    distance = Vector.norm direction
    magnitude = bigG * source.mass * target.mass / distance ^ 2
  in
    if source.position /= target.position then
      Vector.scale magnitude direction
    else
      Vector.zero


potential : Body a -> Body a -> Float
potential source target =
  let
    distance = Vector.norm <| source.position `minus` target.position
  in
    negate <| bigG * source.mass * target.mass / distance


bigG : Float
bigG = 6.674e-11