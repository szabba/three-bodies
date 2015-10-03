module ThreeBodies where

import Dynamics
import Planet exposing (Planet)
import Gravity
import Vector exposing (Vector, plus)

import Pause

import StartApp
import Effects exposing (Effects, Never)
import Task
import Time exposing (Time)

import Html exposing (..)
import Html.Attributes as Attributes
import Html.Events as Events

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
  ( Pause.active (\dt -> Dynamics.update dt >> Dynamics.recenterMass) system
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
    div [ Attributes.id "content" ]
        [ h1 [] [ text "The three body problem" ]
        , p [] [ text problemDescription ]
        , Planet.view 50 (600, 400) system
        , div [] [ pauseButton address model.paused ]
        , authorFooter
        ]


authorFooter : Html
authorFooter =
  div
    [ Attributes.id "footer" ]
    [ avatar ]


avatar : Html
avatar =
  img
    [ Attributes.id "avatar"
    , Attributes.src gravatarURL
    , Attributes.alt "avatar"
    , Attributes.width avatarSize
    , Attributes.height avatarSize
    ]
    []


avatarSize : Int
avatarSize = 80


{- FIXME: Get an MD5 implementation in Elm, so we can just hash an e-mail. -}
gravatarURL : String
gravatarURL =
  "http://www.gravatar.com/avatar/c883fb6c6f1304c1b4b6eb1b0147b792?s=80&d=mm"


pauseButton : Address Action -> Bool -> Html
pauseButton address paused =
  let
    content = if paused then "unpause" else "pause"
  in
    button
      [ Events.onClick address Pause.Toggle ]
      [ text content ]


problemDescription : String
problemDescription =
  String.concat
   [ "In physics and classical mechanics, the three-body problem is the problem"
   , " of taking an initial set of data that specifies the positions, masses "
   , "and velocities of three bodies for some particular point in time and "
   , "then determining the motions of the three bodies, in accordance with the"
   , " laws of classical mechanics (Newton's laws of motion and of universal "
   , "gravitation)." ]