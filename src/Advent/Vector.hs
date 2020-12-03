module Advent.Vector(
    Vector(..)
  , toTuple
  , Advent.Vector.map) where

{- |

Represents a Vector in two dimentions:

* 'x': Horizontal direction
* 'y': Vertical direction

The two dimensions do not have to have the same type.
-}
data Vector a b = Vector { x :: a, y :: b}
  deriving (Eq, Ord)

instance (Show a, Show b) => Show (Vector a  b) where
  show (Vector x y) = "(" ++ show x ++ "," ++ show y ++ ")"

instance (Semigroup a, Semigroup b) => Semigroup (Vector a b) where
  (Vector x1 y1) <> (Vector x2 y2) =
    Vector (x1 <> x2) (y1 <> y2)

toTuple :: Vector a b -> (a, b)
toTuple (Vector x y) = (x, y)

map :: (a -> b) -> (c -> d) -> Vector a c -> Vector b d
map f g (Vector x y) = Vector (f x) (g y) 