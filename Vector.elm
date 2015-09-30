module Vector
  ( Vector, plus, minus, dot, scale ) where

type alias Vector = { x : Float, y : Float }

zero : Vector
zero = { x = 0.0, y = 0.0 }

plus : Vector -> Vector -> Vector
plus a b = { x = a.x + b.x, y = a.y + b.y }

minus : Vector -> Vector -> Vector
minus a b = { x = a.x - b.x, y = a.y - b.y }

dot : Vector -> Vector -> Vector
dot a b = { x = a.x * b.x, y = a.y * b.y }

scale : Float -> Vector -> Vector
scale by {x, y} = { x = by * x, y = by * y }

infixl 6 `plus`
infixl 6 `minus`
infixl 7 `dot`