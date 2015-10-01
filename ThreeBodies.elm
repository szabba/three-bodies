module ThreeBodies where

import Planet
import Gravity
import Vector exposing (Vector, plus)

import Debug exposing (log)

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

app : StartApp.App (List Planet.Model)
app = StartApp.start { init = init, view = view, update = update, inputs = [ticker 0.5] }

-- MODEL

init : (List Planet.Model, Effects a)
init = ( planets, Effects.none )

planets : List Planet.Model
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

ticker : Time -> Signal Planet.Action
ticker dt =
  Time.every (dt * Time.second)
    |> Signal.map (\_ -> { dt = dt })

-- UPDATE

update : Planet.Action -> List Planet.Model -> (List Planet.Model, Effects Planet.Action)
update action planets =
  let
    forceOn planet =
      planets
        |> List.map (Gravity.force planet)
        |> Vector.sum
        |> log ("force on " ++ toString planet)

    accelerationOf planet =
      forceOn planet
        |> Vector.scale (1 / planet.mass)
        |> log ("acceleration of " ++ toString planet)

    accelerate planet =
      let
        {dt} = action
        {velocity} = planet
        acceleration = accelerationOf planet
        newVelocity = velocity `plus` Vector.scale dt acceleration
      in
        { planet | velocity <- newVelocity }
          |> log ("accelerated " ++ toString planet ++ " is")

    acceleratedPlanets = List.map accelerate planets
    movedPlanets = List.map (Planet.update action) acceleratedPlanets
      |> log ("the moved planets are")
  in
    ( movedPlanets, Effects.none )

zip : List a -> List b -> List (a, b)
zip first second =
  case (first, second) of
    (f :: fs, s :: ss) -> (f, s) :: zip fs ss
    _                  -> []

-- VIEW

view : Address Planet.Action -> List Planet.Model -> Html
view _ planets =
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
        , ("font-size", "11pt")
        , ("margin", "40pt auto")
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

planetCanvas : List Planet.Model -> Html
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

scaleFactor : Int -> (Int, Int) -> List Planet.Model -> Float
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