module Planet
  ( Planet, System, view ) where

import Dynamics

import Graphics.Collage as C
import Color exposing (red, black)

type alias System = Dynamics.System { radius : Float }

type alias Planet = Dynamics.Body { radius : Float }

view : Planet -> C.Form
view planet =
  C.circle planet.radius
    |> C.filled red
    |> C.move (planet.position.x, planet.position.y)
