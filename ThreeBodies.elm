module ThreeBodies where

import Html exposing (..)
import Html.Events exposing (..)

import String

main =
  div []
      [ h1 [] [ text "The three body problem" ]
      , p  [] [ text problemDescription ]
      ]

problemDescription =
  String.concat
   [ "In physics and classical mechanics, the three-body problem is the problem"
   , " of taking an initial set of data that specifies the positions, masses "
   , "and velocities of three bodies for some particular point in time and "
   , "then determining the motions of the three bodies, in accordance with the"
   , " laws of classical mechanics (Newton's laws of motion and of universal "
   , "gravitation)." ]
