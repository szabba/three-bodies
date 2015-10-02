module Scale
  ( Meter, fitWithMeter, addMargin, fit ) where

{-| Knows how to extract the dimmensions from a value of type `a`. -}
type alias Meter a = a -> (Int, Int)

{-| Calculates a scaling factor that ensures the value of type `a` will fit
into the specified dimmensions.
-}
fitWithMeter : Meter a -> (Int, Int) -> a -> Float
fitWithMeter sizer dimmensions obj = fit (sizer obj) dimmensions

{-| Calculates the scaling factor needed to add a margin of the specified with
to something with the given dimmensions. Note, that since the aspect ratio is
preserved this will result in a higher margin along one of the axes unless the
object being scaled is square.
-}
addMargin : Int -> (Int, Int) -> Float
addMargin margin dimmensions =
  let
    (width, height) = dimmensions
    safeWidth = width - 2 * margin
    safeHeight = height - 2 * margin
  in
    fit dimmensions (safeWidth, safeHeight)

{-| Calculates how to scale something having the source dimmensions so that it
fits into the target dimmensions.
-}
fit : (Int, Int) -> (Int, Int) -> Float
fit source target =
  let
    (sourceWidth, sourceHeight) = source
    (targetWidth, targetHeight) = target
    xFactor = toFloat targetWidth / toFloat sourceWidth
    yFactor = toFloat targetHeight / toFloat sourceHeight
  in
    min xFactor yFactor