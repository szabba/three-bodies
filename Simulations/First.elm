module Simulations.First where

import Html exposing (..)

import Html.Events as Events
import Signal exposing (Address)

import Trace exposing (Trace)
import Pause
import Time exposing (Time)

import Planet exposing (Planet)
import Dynamics
import Gravity
import Vector

import TimeSeries exposing (TimeSeries)


type alias Model = Pause.Model Time (Trace TracedData Time Planet.System)


type alias TracedData =
  { time : Float
  , dt : Float
  , totalEnergy : Float
  , potentialEnergy : Float
  , kineticEnergy : Float
  }


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


updateTraced : Time -> Trace TracedData Time Planet.System -> Trace TracedData Time Planet.System
updateTraced dt tracedModel =
  Trace.limitTrace 10 <| Trace.update dt tracedModel


traceProjection : Maybe TracedData -> Time -> Planet.System -> TracedData
traceProjection prevTrace dt newState =
  let
    totalPastTime = Maybe.withDefault 0.0 <| Maybe.map .time prevTrace
    totalTime = totalPastTime + dt
    potentialEnergy = Dynamics.potentialEnergy newState
    kineticEnergy = Dynamics.kineticEnergy newState
    totalEnergy = potentialEnergy + kineticEnergy
  in
    { time = totalTime
    , dt = dt
    , totalEnergy = totalEnergy
    , potentialEnergy = potentialEnergy
    , kineticEnergy = kineticEnergy
    }


updateSystem : Time -> Planet.System -> Planet.System
updateSystem dt =
  Dynamics.update dt >> Dynamics.recenterMass


-- VIEW


view : Int -> (Int, Int) -> Address Action -> Model -> List Html
view margin dimmensions address model =
  let
    planetSystem = model.inner.innerModel
    {trace} = model.inner
  in
    [ Planet.view margin dimmensions planetSystem
    , pauseButton address model.paused
    , TimeSeries.view dimmensions <| energyTS .totalEnergy trace
    , TimeSeries.view dimmensions <| energyTS .potentialEnergy trace
    , TimeSeries.view dimmensions <| energyTS .kineticEnergy trace
    , p [] [ text << toString <| model.inner.innerModel ]
    , p [] [ text << toString << List.head <| model.inner.trace ]
    ]


energyTS : (TracedData -> Float) -> List TracedData -> TimeSeries
energyTS proj trace =
  let
    traceDatumToTSDatum datum =
      { dt = datum.dt, value = proj datum }
  in
    trace
      |> List.reverse
      |> List.map traceDatumToTSDatum
      |> List.foldl (flip TimeSeries.append) TimeSeries.empty


pauseButton : Address Action -> Bool -> Html
pauseButton address paused =
  let
    content = if paused then "unpause" else "pause"
  in
    button
      [ Events.onClick address Pause.Toggle ]
      [ text content ]