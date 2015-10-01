module Dynamics
  ( System, Body, ForceSource, update ) where

import Vector exposing (Vector, plus)
import Time exposing (Time)

import Debug exposing (log)

type alias System a =
  { bodies : List (Body a)
  , forceSource : ForceSource a
  }

type alias Body a =
  { a | mass : Float
      , position : Vector
      , velocity : Vector
  }

type alias ForceSource a = List (Body a) -> Body a -> Vector

update : Time -> System a -> System a
update dt system =
  let
    {bodies, forceSource} = system
    acceleratedBodies = List.map (accelerate dt forceSource bodies) bodies
    movedBodies = List.map (move dt) acceleratedBodies
  in
    { system | bodies <- movedBodies }

accelerate : Time -> ForceSource a -> List (Body a) -> Body a -> Body a
accelerate dt forceSource allBodies target =
  let
    {mass, velocity} = target
    force = forceSource allBodies target
    acceleration = Vector.scale (1 / mass) force
    speedup = Vector.scale dt acceleration
    newVelocity = velocity `plus` speedup
  in
    { target | velocity <- newVelocity }

move : Time -> Body a -> Body a
move dt body =
  let
    {position, velocity} = body
    displacement = Vector.scale dt velocity
    newPosition = position `plus` displacement
  in
    { body | position <- newPosition }