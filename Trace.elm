module Trace
  ( Trace, new, recordModel, update, limitTrace ) where


type alias Trace t a m =
  { innerModel : m
  , update : a -> m -> m
  , trace : t
  , project : t -> a -> m -> t
  }


new : t -> (t -> a -> m -> t) -> (a -> m -> m) -> m -> Trace t a m
new initialTrace projection update innerModel =
  { innerModel = innerModel
  , update = update
  , trace = initialTrace
  , project = projection
  }


recordModel : (List m) -> a -> m -> (List m)
recordModel trace _ innerModel =
  trace ++ [ innerModel ]


update : a -> Trace t a m -> Trace t a m
update action model =
  let
    {innerModel, update, trace, project} = model
    updatedModel = update action innerModel
    newTrace = project trace action updatedModel
  in
    { model | innerModel <- updatedModel , trace <- newTrace }


limitTrace : Int -> Trace (List t) a m -> Trace (List t) a m
limitTrace maxLength model =
  { model | trace <- List.take maxLength model.trace }