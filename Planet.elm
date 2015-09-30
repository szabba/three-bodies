module Planet
  ( Planet, view ) where

import Html exposing (Html)
import Html

import Graphics.Element as G
import Graphics.Collage as C

import Color exposing (red, black)

type alias Planet = { x : Float, y : Float, mass : Float, radius : Float }

view : Planet -> C.Form
view planet =
  C.circle planet.radius
    |> C.filled red
    |> C.move (planet.x, planet.y)
