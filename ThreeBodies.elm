module ThreeBodies where

import Planet

import StartApp
import Effects exposing (Effects, Never)
import Task
import Time exposing (Time)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (style)

import Color exposing (red, black)

import Graphics.Element as G
import Graphics.Collage as C

import Signal exposing (Signal, Address)
import String

main = app.html

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks

app = StartApp.start { init = init, view = view, update = update, inputs = [ticker 0.5] }

init = ( planets, Effects.none )

update : Planet.Action -> List Planet.Model -> (List Planet.Model, Effects Planet.Action)
update action planets =
  let
    updates = List.map (Planet.update action) planets
    planets' = List.map (\(planet, _) -> planet) updates
    effects = List.map (\(_, effect) -> effect) updates
  in
    ( planets', Effects.batch effects )

view : Address Planet.Action -> List Planet.Model -> Html
view _ planets =
  div [ containerStyle ]
      [ h1 [] [ text "The three body problem" ]
      , p  [] [ text problemDescription ]
      , planetCanvas planets
      ]

ticker : Time -> Signal Planet.Action
ticker dt =
  Time.every (dt * Time.second)
    |> Signal.map (\_ -> { dt = dt })

containerStyle =
  style [ ("width",     "40em")
        , ("font-family", "sans-serif")
        , ("font-size", "11pt")
        , ("margin", "40pt auto")
        ]

planets =
  [ { x =   0.0, y = -20.0, mass = 10.0, radius = 15.0 }
  , { x = 100.0, y =   0.0, mass = 10.0, radius = 20.0 }
  , { x = -70.0, y =  60.0, mass = 10.0, radius = 25.0 }
  ]

problemDescription =
  String.concat
   [ "In physics and classical mechanics, the three-body problem is the problem"
   , " of taking an initial set of data that specifies the positions, masses "
   , "and velocities of three bodies for some particular point in time and "
   , "then determining the motions of the three bodies, in accordance with the"
   , " laws of classical mechanics (Newton's laws of motion and of universal "
   , "gravitation)." ]


planetCanvas : List Planet.Model -> Html
planetCanvas planets =
  let
    (width, height) = sizeOfCanvas 40 planets
  in
    List.map Planet.view planets
      |> C.collage width height
      |> G.color black
      |> Html.fromElement

sizeOfCanvas : Int -> List Planet.Model -> (Int, Int)
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
      |> \maxPos -> maxPos + margin
  in
    (ensureFit .x, ensureFit .y)