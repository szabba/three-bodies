module Planet
  ( Model, Action, update, view ) where

import Graphics.Collage as C

import Color exposing (red, black)

import Vector exposing (Vector, plus)

type alias Model = { position : Vector, velocity : Vector, mass : Float, radius : Float }

type alias Action = { dt : Float }

update : Action -> Model -> Model
update {dt} planet =
  let
    {position, velocity} = planet
    displacement = Vector.scale dt velocity
    newPosition = position `plus` displacement
  in
    { planet | position <- newPosition }

view : Model -> C.Form
view planet =
  C.circle planet.radius
    |> C.filled red
    |> C.move (planet.position.x, planet.position.y)
