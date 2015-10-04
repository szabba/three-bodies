module Trace
  ( Trace, new, newWithTrace, newWithProjection, new', recordModel
  , update, limitTrace
  ) where


type alias Trace t a m =
  { innerModel : m
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
new' initialTrace projection update innerModel =
  { innerModel = innerModel
  , update = update
  , trace = initialTrace
  , project = projection
  }


recordModel : Maybe m -> a -> m -> m
recordModel _ _ innerModel =
  innerModel


update : a -> Trace t a m -> Trace t a m
update action model =
  let
    {innerModel, update, trace, project} = model
    updatedModel = update action innerModel
    newTrace = project (List.head trace) action updatedModel :: trace
  in
    { model | innerModel <- updatedModel , trace <- newTrace }


limitTrace : Int -> Trace t a m -> Trace t a m
limitTrace maxLength model =
  { model | trace <- List.take maxLength model.trace }