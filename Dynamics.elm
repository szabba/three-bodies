module Dynamics
  ( System, Body, ForceSource, update, recenterMass, totalEnergy ) where

import Vector exposing (Vector, plus, minus)
import Time exposing (Time)


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
    bodiesWithSources = findSourcesOfForces bodies
    acceleratedBodies = List.map (uncurry <| accelerate dt forceSource) bodiesWithSources
    movedBodies = List.map (move dt) acceleratedBodies
  in
    { system | bodies <- movedBodies }


findSourcesOfForces : List (Body a) -> List (List (Body a), Body a)
findSourcesOfForces bodies =
  let
    indexedBodies = List.indexedMap (,) bodies
    bodyWithSources indexedBody =
      let
        (index, body) = indexedBody
        sourceOrNone indexedSource =
          let
            (index', source) = indexedSource
          in
            if index == index' then Nothing else Just source
        sources = List.filterMap sourceOrNone indexedBodies
      in
        (sources, body)
  in
    List.map bodyWithSources indexedBodies



totalEnergy : (System a -> Float) -> System a -> Float
totalEnergy potentialEnergy system =
  let
    kineticEnergy = List.sum <| List.map bodyKineticEnergy system.bodies
  in
    kineticEnergy + potentialEnergy system


bodyKineticEnergy : Body a -> Float
bodyKineticEnergy body =
  let
    {mass, velocity} = body
  in
    mass * Vector.norm velocity ^ 2 / 2


recenterMass : System a -> System a
recenterMass system =
  let
    {bodies} = system
    centerOfMass = findCenterOfMass bodies
    recenter body = { body | position <- body.position `minus` centerOfMass }
    movedBodies = List.map recenter bodies
  in
    { system | bodies <- movedBodies }


findCenterOfMass : List (Body a) -> Vector
findCenterOfMass bodies =
  let
    totalMass = List.sum <| List.map .mass bodies
  in
    bodies
      |> List.map weightPosition
      |> Vector.sum
      |> Vector.scale (1 / totalMass)


weightPosition : Body a -> Vector
weightPosition body =
  Vector.scale body.mass body.position


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