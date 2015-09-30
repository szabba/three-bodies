module Planet
  ( Model, Action, update, view ) where

import Effects exposing (Effects)

import Html exposing (Html)
import Html

import Graphics.Element as G
import Graphics.Collage as C

import Color exposing (red, black)

type alias Model = { x : Float, y : Float, mass : Float, radius : Float }

type alias Action = { dt : Float }

update : Action -> Model -> (Model, Effects Action)
update {dt} planet =
  let
    { x, y } = planet
  in
    ( { planet | x <- x + 1.0 * dt
               , y <- y - 1.0 * dt }
    , Effects.none
    )

view : Model -> C.Form
view planet =
  C.circle planet.radius
    |> C.filled red
    |> C.move (planet.x, planet.y)
