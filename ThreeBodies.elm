module ThreeBodies where

import Layout.Footer as Footer

import Simulations.First as First

import Pause

import StartApp
import Effects exposing (Effects, Never)
import Task
import Time exposing (Time)

import Html exposing (..)
import Html.Attributes as Attributes

import Signal exposing (Signal, Address)
import String


main : Signal Html
main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks


app : StartApp.App First.Model
app =
  StartApp.start
    { init = init
    , view = view
    , update = update
    , inputs = [ticker 0.05]
    }


-- MODEL


init : (First.Model, Effects First.Action)
init =
  ( First.init, Effects.none )


ticker : Time -> Signal First.Action
ticker dt =
  dt * Time.second
    |> Time.every
    |> Signal.map (Pause.Wrapped << always dt)


-- UPDATE


update :  First.Action -> First.Model -> (First.Model, Effects First.Action)
update action model =
  ( First.update action model, Effects.none )


-- VIEW


view : Address First.Action -> First.Model -> Html
view address model =
  let
    system = model.inner
    header = h1 [] [ text "The three body problem" ]
    problem = p [] [ text problemDescription ]
    firstSimulation = First.view 50 (600, 400) address model
    content = [ header, problem ] ++ firstSimulation ++ [ text (toString model), todo, Footer.view ]
  in
    div [ Attributes.id "content" ] content


todo : Html
todo =
  [ "Protect against NaN/infinite forces"
  , "Show a tail / trace trajectories"
  , "Factory reset button"
  , "Plot total energy over time"
  , "Random configuration button"
  , "Discuss constrained forms of the problem"
  , "Visualize the instability (Lyapunov exponents!)"
  ]
    |> List.map (\todoItem -> li [] [ text todoItem ])
    |> ul []


problemDescription : String
problemDescription =
  String.concat
   [ "In physics and classical mechanics, the three-body problem is the problem"
   , " of taking an initial set of data that specifies the positions, masses "
   , "and velocities of three bodies for some particular point in time and "
   , "then determining the motions of the three bodies, in accordance with the"
   , " laws of classical mechanics (Newton's laws of motion and of universal "
   , "gravitation)." ]