module Planet
  ( Model, Action, update, view ) where

import Effects exposing (Effects)

import Html exposing (Html)
import Html

import Graphics.Element as G
import Graphics.Collage as C

import Color exposing (red, black)

import Vector exposing (Vector, plus)

type alias Model = { position : Vector, mass : Float, radius : Float }

type alias Action = { dt : Float }

update : Action -> Model -> (Model, Effects Action)
update {dt} planet =
  let
    velocity = { x = 1.0, y = -1.0 }
    displacement = Vector.scale dt velocity
    newPosition = planet.position `plus` displacement
  in
    ( { planet | position <- newPosition }
    , Effects.none
    )

view : Model -> C.Form
view planet =
  C.circle planet.radius
    |> C.filled red
    |> C.move (planet.position.x, planet.position.y)
