module ThreeBodies where

import Dynamics
import Planet exposing (Planet)
import Gravity
import Vector exposing (Vector, plus)

import Planet.Scale as PS
import Scale

import StartApp
import Effects exposing (Effects, Never)
import Task
import Time exposing (Time)

import Html exposing (..)
import Html.Attributes exposing (style)

import Color exposing (red, black)

import Graphics.Element as G
import Graphics.Collage as C

import Signal exposing (Signal, Address)
import String

main : Signal Html
main = app.html

port tasks : Signal (Task.Task Never ())
port tasks = app.tasks

app : StartApp.App Planet.System
app = StartApp.start { init = init, view = view, update = update, inputs = [ticker 0.05] }

-- MODEL

init : (Planet.System, Effects a)
init = ( system, Effects.none )

system : Planet.System
system =
  { bodies = planets
  , forceSource = Gravity.force
  }

planets : List Planet
planets =
  [ { position = { x = 0.0, y = -20.0 }
    , velocity = Vector.zero
    , mass = 1e15
    , radius = 15.0 }
  , { position = { x = 100.0, y = 0.0 }
    , velocity = Vector.zero
    , mass = 1e15
    , radius = 20.0 }
  , { position = { x = -70.0, y = 60.0 }
    , velocity = Vector.zero
    , mass = 1e15
    , radius = 25.0 }
  ]

ticker : Time -> Signal Time
ticker dt =
  dt * Time.second
    |> Time.every
    |> Signal.map (always dt)

-- UPDATE

update : Time -> Planet.System -> (Planet.System, Effects Time)
update dt system =
  let
    updatedSystem = Dynamics.update dt system
  in
    (updatedSystem, Effects.none)

zip : List a -> List b -> List (a, b)
zip first second =
  case (first, second) of
    (f :: fs, s :: ss) -> (f, s) :: zip fs ss
    _                  -> []

-- VIEW

view : Address Time -> Planet.System -> Html
view _ system =
  div [ containerStyle ]
      [ h1 [] [ text "The three body problem" ]
      , p  [] [ text problemDescription ]
      , planetCanvas system
      , p  [] [ text <| toString system.bodies ]
      ]

containerStyle : Attribute
containerStyle =
  style [ ("width",     "40em")
        , ("font-family", "sans-serif")
        , ("font-size", "15px")
        , ("margin", "40px auto")
        ]

problemDescription : String
problemDescription =
  String.concat
   [ "In physics and classical mechanics, the three-body problem is the problem"
   , " of taking an initial set of data that specifies the positions, masses "
   , "and velocities of three bodies for some particular point in time and "
   , "then determining the motions of the three bodies, in accordance with the"
   , " laws of classical mechanics (Newton's laws of motion and of universal "
   , "gravitation)." ]

planetCanvas : Planet.System -> Html
planetCanvas system =
  let
    width = 600
    height = 400
    margin = 30
    scaleBy = scaleFactor margin (width, height) system

    planetShapes =
      List.map Planet.view system.bodies
        |> C.group
        |> C.scale scaleBy
  in
    [planetShapes]
      |> C.collage width height
      |> G.color black
      |> Html.fromElement

scaleFactor : Int -> (Int, Int) -> Planet.System -> Float
scaleFactor margin targetDimmensions system =
  let
    marginFactor = Scale.addMargin margin targetDimmensions
    fitFactor = Scale.fitWithMeter PS.meter targetDimmensions system
  in
    marginFactor * fitFactor
