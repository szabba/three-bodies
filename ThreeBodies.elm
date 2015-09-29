module ThreeBodies where

import Planet

import Html exposing (..)
import Html.Events exposing (..)

import Color exposing (red, black)

import Graphics.Element as G
import Graphics.Collage as C

import String

main =
  div []
      [ h1 [] [ text "The three body problem" ]
      , p  [] [ text problemDescription ]
      , Planet.view planets
      ]

planets =
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
