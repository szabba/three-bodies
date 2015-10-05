module TimeSeries
  ( TimeSeries, empty, append ) where

import Maybe


{-| A TimeSeries stores historical data describing how a scalar value has
changed throughout time.
-}
type TimeSeries =
  TimeSeries
    { totalTime : Maybe Float
    , maxValue : Maybe Float
    , minValue : Maybe Float
    , dataPoints : List (Float, Float)
    }


{-| A time series that doesn't contain any data yet.
-}
empty : TimeSeries
empty =
  TimeSeries
    { totalTime = Nothing
    , minValue = Nothing
    , maxValue = Nothing
    , dataPoints = []
    }

{-| Appends a data point to a time series. -}
append : TimeSeries -> { dt : Float, value : Float } -> TimeSeries
append timeSeries {dt, value} =
  let
    record = case timeSeries of TimeSeries ts -> ts
    {totalTime, minValue, maxValue, dataPoints} = record
    newDataPoint = (dt, value)
  in
    TimeSeries
      { record
      | totalTime <- updateMaybe dt (+) totalTime
      , minValue <- updateMaybe value min minValue
      , maxValue <- updateMaybe value max maxValue
      , dataPoints <- newDataPoint :: dataPoints
      }


{-| Update a monoidal value wrapped in a maybe using the specified mappend and
incoming value. If the maybe is empty, the new value gets stored in the result.
-}
updateMaybe : a -> (a -> a -> a) -> Maybe a -> Maybe a
updateMaybe new mappend old =
  old
    |> Maybe.map (mappend new)
    |> Maybe.withDefault new
    |> Maybe.Just