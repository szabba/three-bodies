module ThreeBodies where

import Html exposing (..)
import Html.Events exposing (..)

import Color exposing(red)

import Graphics.Element as G
import Graphics.Collage as C

import String

main =
  div []
      [ h1 [] [ text "The three body problem" ]
      , p  [] [ text problemDescription ]
      , view model
      ]

view model =
  let
    planet = C.filled red (C.circle 30)
    combined = C.collage 100 100 [planet]
  in fromElement combined

model =
  [ { x = 0,   y = 0,   mass = 10, radius = 30}
  , { x = 100, y = 0,   mass = 10, radius = 30}
  , { x = 0,   y = 200, mass = 10, radius = 30}
  ]

problemDescription =
  String.concat
   [ "In physics and classical mechanics, the three-body problem is the problem"
   , " of taking an initial set of data that specifies the positions, masses "
   , "and velocities of three bodies for some particular point in time and "
   , "then determining the motions of the three bodies, in accordance with the"
   , " laws of classical mechanics (Newton's laws of motion and of universal "
   , "gravitation)." ]
