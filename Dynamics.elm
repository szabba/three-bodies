module Dynamics
  ( System, Body, Interaction, update, recenterMass, totalEnergy
  , potentialEnergy, kineticEnergy ) where

import Vector exposing (Vector, plus, minus)
import Time exposing (Time)


type alias System a =
  { bodies : List (Body a)
  , interaction : Interaction a
  }


type alias Body a =
  { a | mass : Float
      , position : Vector
      , velocity : Vector
  }


type alias Interaction a =
  { source : Body a, target : Body a } -> { potential : Float, force : Vector }


-- ENERGY


totalEnergy : System a -> Float
totalEnergy system =
  potentialEnergy system + kineticEnergy system


potentialEnergy : System a -> Float
potentialEnergy {bodies, interaction} =
  let
    pairsWithFirst i body = List.map ((,) body) <| List.drop (i + 1) bodies
    pairs = List.concat <| List.indexedMap pairsWithFirst bodies
    pairwise (source, target) =
      { source = source, target = target }
        |> interaction
        |> .potential
  in
    List.sum <| List.map pairwise pairs



kineticEnergy : System a -> Float
kineticEnergy {bodies} =
  List.sum <| List.map bodyKineticEnergy bodies


bodyKineticEnergy : Body a -> Float
bodyKineticEnergy body =
  let
    {mass, velocity} = body
  in
    mass * Vector.norm velocity ^ 2 / 2


-- UPDATE


update : Time -> System a -> System a
update dt system =
  let
    bodiesWithSources = findSourcesOfForces system.bodies
    acceleratedBodies = List.map (accelerate dt system.interaction) bodiesWithSources
    movedBodies = List.map (move dt) acceleratedBodies
  in
    { system | bodies <- movedBodies }


findSourcesOfForces : List (Body a) -> List (List (Body a), Body a)
findSourcesOfForces bodies =
  let
    sourcesFor n =
      List.take (n - 1) bodies ++ List.drop n bodies
    bodyWtihSources n body =
      (sourcesFor n, body)
  in
    List.indexedMap bodyWtihSources bodies


accelerate : Time -> Interaction a -> (List (Body a), Body a) -> Body a
accelerate dt interaction (sources, target) =
  let
    {mass, velocity} = target
    force =
      sources
        |> List.map (\source -> interaction { source = source, target = target })
        |> List.map .force
        |> Vector.sum
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


-- CENTER OF MASS


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