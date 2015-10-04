module Simulations.First where

import Html exposing (..)
import Html.Events as Events

import Plot exposing (Plot)
import Graphics.Collage as Collage
import Color

import Planet exposing (Planet)
import Dynamics
import Gravity
import Vector

import Trace exposing (Trace)
import Pause
import Time exposing (Time)
import Signal exposing (Address)


type alias Model = Pause.Model Time (Trace TracedData Time Planet.System)


type alias TracedData =
  { totalTime : Float }


type alias Action = Pause.Action Time


-- MODEL


init : Model
init =
  Pause.active Trace.update tracedSystem


tracedSystem : Trace TracedData Time Planet.System
tracedSystem =
  Trace.newWithProjection traceProjection updateSystem system


system : Planet.System
system =
  { bodies = planets
  , interaction = Gravity.interaction
  }


planets : List Planet
planets =
  [ { position = { x = 0.0, y = -20.0 }
    , velocity = Vector.zero
    , mass = 5e15
    , radius = 50.0
    }
  , { position = { x = 100.0, y = 0.0 }
    , velocity = Vector.zero
    , mass = 1e15
    , radius = 20.0
    }
  , { position = { x = -70.0, y = 60.0 }
    , velocity = Vector.zero
    , mass = 1e15
    , radius = 20.0
    }
  ]


-- UPDATE


update : Action -> Model -> Model
update =
  Pause.update


traceProjection : Maybe TracedData -> Time -> Planet.System -> TracedData
traceProjection prevTrace dt newState =
  let
    totalPastTime = Maybe.withDefault 0.0 <| Maybe.map .totalTime prevTrace
    totalTime = totalPastTime + dt
  in
    { totalTime = totalTime }


updateSystem : Time -> Planet.System -> Planet.System
updateSystem dt =
  Dynamics.update dt >> Dynamics.recenterMass


-- VIEW


view : Int -> (Int, Int) -> Address Action -> Model -> List Html
view margin dimmensions address model =
  [ Planet.view margin dimmensions model.inner.model
  --, energyPlot [(0, 0), (100, 100), (200, 0)]
  , pauseButton address model.paused
  ]

energyPlot : Plot -> Html
energyPlot plot =
  let
    width = 600
    height = 400
    margin = 10
    lineStyle = Collage.solid Color.red
  in
    plot
     |> Plot.view lineStyle (width, height)
     |> List.repeat 1
     |> Collage.collage width height
     |> Html.fromElement


pauseButton : Address Action -> Bool -> Html
pauseButton address paused =
  let
    content = if paused then "unpause" else "pause"
  in
    button
      [ Events.onClick address Pause.Toggle ]
      [ text content ]