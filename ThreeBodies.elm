module ThreeBodies where

import Dynamics
import Planet exposing (Planet)
import Gravity
import Vector exposing (Vector, plus)

import Planet.Scale as PS
import Scale

import Pause

import StartApp
import Effects exposing (Effects, Never)
import Task
import Time exposing (Time)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

import Color exposing (red, black)

import Graphics.Element as G
import Graphics.Collage as C

import Signal exposing (Signal, Address)
import String


type alias Model =
  Pause.Model Time Planet.System


type alias Action =
  Pause.Action Time


main : Signal Html
main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks


app : StartApp.App Model
app =
  StartApp.start
    { init = init
    , view = view
    , update = update
    , inputs = [ticker 0.05]
    }


-- MODEL


init : (Model, Effects Action)
init =
  ( Pause.active Dynamics.update system
  , Effects.none
  )


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
    , radius = 15.0
    }
  , { position = { x = 100.0, y = 0.0 }
    , velocity = Vector.zero
    , mass = 1e15
    , radius = 20.0
    }
  , { position = { x = -70.0, y = 60.0 }
    , velocity = Vector.zero
    , mass = 1e15
    , radius = 25.0
    }
  ]


ticker : Time -> Signal Action
ticker dt =
  dt * Time.second
    |> Time.every
    |> Signal.map (Pause.Wrapped << always dt)


-- UPDATE


update :  Action -> Model -> (Model, Effects Action)
update action model =
  ( Pause.update action model
  , Effects.none
  )


-- VIEW


view : Address Action -> Model -> Html
view address model =
  let
    system = model.inner
  in
    div [ containerStyle ]
        [ h1 [] [ text "The three body problem" ]
        , p  [] [ text problemDescription ]
        , planetCanvas system
        , pauseButton address model.paused
        , p  [] [ text <| toString system.bodies ]
        ]


pauseButton : Address Action -> Bool -> Html
pauseButton address paused =
  let
    content = if paused then "unpause" else "pause"
  in
    button [ onClick address Pause.Toggle ] [ text content ]


containerStyle : Attribute
containerStyle =
  style [ ("width", "40em")
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