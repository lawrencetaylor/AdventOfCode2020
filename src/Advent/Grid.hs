{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}

module Advent.Grid(
    Position(..)
  , Grid
  , readGridFromDay
  , maxPos
  , mapFst
  , Advent.Grid.map
  , gridString
  , neighbours
  , kingMoves) where

import           Advent.Parsing  (readDay)
import qualified Advent.Vector   as V (Vector (Vector), map, toTuple)
import           Data.List       (groupBy, intercalate, sortOn)
import           Data.Map.Strict ((!?), member, Map, findMax, toList)
import qualified Data.Map.Strict as Map
import           Data.Tuple
import Data.Semigroup (Sum(Sum))
import Data.Maybe (catMaybes)
import Data.Maybe (mapMaybe)

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

kingMoves :: [V.Vector (Sum Int) (Sum Int)]
kingMoves = 
  [ V.Vector 1 0 
  , V.Vector 1 1
  , V.Vector 0 1
  , V.Vector (-1) 1
  , V.Vector (-1) 0
  , V.Vector (-1) (-1)
  , V.Vector 0 (-1)
  , V.Vector 1 (-1)
  ]

neighbours :: Grid (Sum Int) (Sum Int) -> Position (Sum Int) (Sum Int) ->  [(Position (Sum Int) (Sum Int), Char)]
neighbours grid pos = mapMaybe (lookupWithKey grid  . (<> pos) ) kingMoves
  where
    lookupWithKey m k = (k,) <$>  m !? k

gridString :: Grid Int Int -> String
gridString grid = intercalate ['\n'] $ fmap snd  <$> lines
  where
    lines =
      groupBy (\ x y -> sameY (fst x) (fst y)) $
      sortOn (swap . V.toTuple . fst)
      $ toList grid
    sameY (V.Vector  _ a) ( V.Vector _ b) = a == b
