{-# LANGUAGE FlexibleInstances #-}

module Advent.Grid(
    Position(..)
  , Grid
  , readGridFromDay
  , maxPos
  , mapFst
  , Advent.Grid.map) where

import           Advent.Parsing  (readDay)
import qualified Advent.Vector   as V (Vector (Vector), map, toTuple)
import           Data.Map.Strict (Map, findMax)
import qualified Data.Map.Strict as Map

type Position a b = V.Vector a b

{- |
A grid of characters in indexed by a set of "Position"s.
-}
type Grid a b = Map (Position a b) Char

readGridFromDay :: Int -> IO (Grid Int Int)
readGridFromDay day = do
  text <- readDay day
  let gridLines = lines text
  return $
    Map.fromList
    [ (V.Vector i j, c ) |
          (line, j) <- zip gridLines [0..]
        , (c, i) <- zip line [0..]
    ]

maxPos :: Grid a b  -> (a, b)
maxPos grid = V.toTuple $ fst $ findMax grid

map :: (Ord b, Ord d) => (a -> b) -> (c -> d) -> Grid a c -> Grid b d
map f g = Map.mapKeys (V.map f g)

mapFst :: (Ord b, Ord c) => (a -> b) -> Grid a c -> Grid b c
mapFst f = Advent.Grid.map f id


