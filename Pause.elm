module Pause where


type alias Model a m =
  { inner : m
  , update : a -> m -> m
  , paused : Bool
  }


type Action a = Wrapped a | Toggle


paused : (a -> m -> m) -> m -> Model a m
paused = new True


active : (a -> m -> m) -> m -> Model a m
active = new False


new : Bool -> (a -> m -> m) -> m -> Model a m
new paused update model =
  { inner = model
  , update = update
  , paused = paused
  }


update : Action a -> Model a m -> Model a m
update wrappedAction model =
  case wrappedAction of
    Toggle ->
        { model | paused <- not model.paused }
    Wrapped action ->
        if model.paused
          then model
          else
            { model | inner <- model.update action model.inner }