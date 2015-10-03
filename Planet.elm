module Planet
  ( Planet, System, view ) where

import Dynamics
import Vector exposing (Vector)
import Scale exposing (Meter)

import Html exposing (Html)
import Graphics.Element as Element
import Graphics.Collage as Collage
import Color exposing (red, black)

type alias System = Dynamics.System { radius : Float }

type alias Planet = Dynamics.Body { radius : Float }


view : Int -> (Int, Int) -> System -> Html
view margin dimmensions system =
  let
    (width, height) = dimmensions
    scaleBy = scaleFactor margin dimmensions system

    planetShapes = system.bodies
      |> List.map viewPlanet
      |> Collage.group
      |> Collage.scale scaleBy
  in
    [planetShapes]
      |> Collage.collage width height
      |> Element.color black
      |> Html.fromElement


viewPlanet : Planet -> Collage.Form
viewPlanet planet =
  Collage.circle planet.radius
    |> Collage.filled red
    |> Collage.move (planet.position.x, planet.position.y)


scaleFactor : Int -> (Int, Int) -> System -> Float
scaleFactor margin targetDimmensions system =
  let
    marginFactor = Scale.addMargin margin targetDimmensions
    fitFactor = Scale.fitWithMeter meter targetDimmensions system
  in
    marginFactor * fitFactor


meter : Meter System
meter system =
  let
    xs = List.map (reaches .x >> ceiling) system.bodies
    ys = List.map (reaches .y >> ceiling) system.bodies

    dimmensionAlongAxisWithCoordinates coordinates =
        coordinates
          |> absMaximum 0
          |> \half -> 2 * half

    width = xs |> dimmensionAlongAxisWithCoordinates
    height = ys |> dimmensionAlongAxisWithCoordinates
  in
   (width, height)


reaches : (Vector -> Float) -> Planet -> Float
reaches along planet =
  let
    {position, radius} = planet
    coordinate = along position
    sign = if coordinate < 0 then -1 else 1
  in
    sign * radius + coordinate


absMaximum : comparable -> List comparable -> comparable
absMaximum zero coll =
  coll
    |> List.map abs
    |> List.maximum
    |> Maybe.withDefault zero