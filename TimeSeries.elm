module TimeSeries
  ( TimeSeries, empty, append ) where

import Maybe


{-| A TimeSeries stores historical data describing how a scalar value has
changed throughout time.
-}
type alias TimeSeries =
  { totalTime : Maybe Float
  , maxValue : Maybe Float
  , minValue : Maybe Float
  , dataPoints : List (Float, Float)
  }


{-| A time series that doesn't contain any data yet.
-}
empty : TimeSeries
empty =
  { totalTime = Nothing
  , minValue = Nothing
  , maxValue = Nothing
  , dataPoints = []
  }

{-| Appends a data point to a time series. -}
append : TimeSeries -> { dt : Float, value : Float } -> TimeSeries
append timeSeries {dt, value} =
  let
    {totalTime, minValue, maxValue, dataPoints} = timeSeries
    newTotalTime = updateMaybe dt (+) totalTime
    newDataPoint = (newTotalTime, value)
  in
    { timeSeries
    | totalTime <- Maybe.Just newTotalTime
    , minValue <- Maybe.Just <| updateMaybe value min minValue
    , maxValue <- Maybe.Just <| updateMaybe value max maxValue
    , dataPoints <- dataPoints ++ [newDataPoint]
    }


updateMaybe : a -> (a -> a -> a) -> Maybe a -> a
updateMaybe new mappend old =
  old
    |> Maybe.map (mappend new)
    |> Maybe.withDefault new