module Trace
  ( Trace, new, newWithTrace, newWithProjection, new'
  , recordModel, update ) where


type alias Trace t a m =
  { model : m
  , update : a -> m -> m
  , trace : List t
  , project : Maybe t -> a -> m -> t
  }


new : (a -> m -> m) -> m -> Trace m a m
new =
  new' [] recordModel


newWithTrace : List m -> (a -> m -> m) -> m -> Trace m a m
newWithTrace initialTrace =
  new' initialTrace recordModel


newWithProjection : (Maybe t -> a -> m -> t) -> (a -> m -> m) -> m -> Trace t a m
newWithProjection =
  new' []


new' : List t -> (Maybe t -> a -> m -> t) -> (a -> m -> m) -> m -> Trace t a m
new' initialTrace projection update model =
  { model = model
  , update = update
  , trace = initialTrace
  , project = projection
  }


recordModel : Maybe m -> a -> m -> m
recordModel _ _ model =
  model


update : a -> Trace t a m -> Trace t a m
update action traced =
  let
    {model, update, trace, project} = traced
    updatedModel = update action model
    newTrace = project (List.head trace) action updatedModel :: trace
  in
    { traced
    | model <- updatedModel
    , trace <- newTrace
    }