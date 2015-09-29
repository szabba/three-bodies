module ThreeBodies where

import Html exposing (..)
import Html.Events exposing (..)

import Color exposing(red, black)

import Graphics.Element as G
import Graphics.Collage as C

import String

main =
  div []
      [ h1 [] [ text "The three body problem" ]
      , p  [] [ text problemDescription ]
      , view model
      ]

type alias Planet = { x : Float, y : Float, mass : Float, radius : Float }

view model =
  let
    (width, height) = sizeOfCanvas 40 model
  in
    List.map viewPlanet model
      |> C.collage width height
      |> G.color black
      |> fromElement

sizeOfCanvas margin planets =
  let
    absMaxOr default selector = planets
      |> List.map selector
      |> List.map abs
      |> List.maximum
      |> Maybe.withDefault default

    maxRadius = absMaxOr 0.0 .radius

    ensureFit selector = absMaxOr 0.0 selector
      |> \maxAbsValue -> 2.0 * (maxAbsValue + maxRadius)
      |> ceiling
      |> (\maxPos -> maxPos + margin)
  in
    (ensureFit .x, ensureFit .y)

viewPlanet planet =
  C.circle planet.radius
    |> C.filled red
    |> C.move (planet.x, planet.y)

model =
  [ { x =   0.0, y = -20.0, mass = 10.0, radius = 30.0}
  , { x = 100.0, y =   0.0, mass = 10.0, radius = 30.0}
  , { x = -70.0, y =  60.0, mass = 10.0, radius = 30.0}
  ]

problemDescription =
  String.concat
   [ "In physics and classical mechanics, the three-body problem is the problem"
   , " of taking an initial set of data that specifies the positions, masses "
   , "and velocities of three bodies for some particular point in time and "
   , "then determining the motions of the three bodies, in accordance with the"
   , " laws of classical mechanics (Newton's laws of motion and of universal "
   , "gravitation)." ]
