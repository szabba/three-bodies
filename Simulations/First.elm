module Simulations.First where

import Html exposing (..)
import Html.Events as Events

import Planet exposing (Planet)
import Dynamics
import Gravity
import Vector

import Pause
import Time exposing (Time)
import Signal exposing (Address)


type alias Model = Pause.Model Time Planet.System


type alias Action = Pause.Action Time


-- MODEL


init : Model
init =
  Pause.active (\dt -> Dynamics.update dt >> Dynamics.recenterMass) system


system : Planet.System
system =
  { bodies = planets
  , forceSource = Gravity.force
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


-- VIEW


view : Int -> (Int, Int) -> Address Action -> Model -> List Html
view margin dimmensions address model =
  [ Planet.view margin dimmensions model.inner
  , pauseButton address model.paused
  ]


pauseButton : Address Action -> Bool -> Html
pauseButton address paused =
  let
    content = if paused then "unpause" else "pause"
  in
    button
      [ Events.onClick address Pause.Toggle ]
      [ text content ]