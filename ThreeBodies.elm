module ThreeBodies where

import Dynamics
import Planet exposing (Planet)
import Gravity
import Vector exposing (Vector, plus)

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
  let
    planets = system.bodies
  in
    div [ containerStyle ]
        [ h1 [] [ text "The three body problem" ]
        , p  [] [ text problemDescription ]
        , planetCanvas planets
        , p  [] [ text <| toString planets ]
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

planetCanvas : List Planet -> Html
planetCanvas planets =
  let
    width = 600
    height = 400
    scaleBy = scaleFactor 10 (width, height) planets

    planetShapes =
      List.map Planet.view planets
        |> C.group
        |> C.scale scaleBy
  in
    [planetShapes]
      |> C.collage width height
      |> G.color black
      |> Html.fromElement

scaleFactor : Int -> (Int, Int) -> List Planet -> Float
scaleFactor margin (width, height) planets =
  let
    safeXRange = max 0 ((width - margin) // 2) |> toFloat
    safeYRange = max 0 ((height - margin) // 2) |> toFloat

    maxRadius = absMaximum 0.0 .radius planets

    maxDistanceFromCenter : (Vector -> Float) -> Float
    maxDistanceFromCenter alongAxis =
      absMaximum 0.0 (.position >> alongAxis) planets

    scaleThatEnsuresFit : Float -> (Vector -> Float) -> Float
    scaleThatEnsuresFit safeRange axis =
      scaleToFrom safeRange <| maxDistanceFromCenter axis + maxRadius
  in
    min (scaleThatEnsuresFit safeXRange .x) (scaleThatEnsuresFit safeYRange .y)

scaleToFrom : Float -> Float -> Float
scaleToFrom target origin = target / origin

absMaximum : comparable -> (a -> comparable) -> List a -> comparable
absMaximum zero f coll =
  coll
    |> List.map (f >> abs)
    |> List.maximum
    |> Maybe.withDefault zero