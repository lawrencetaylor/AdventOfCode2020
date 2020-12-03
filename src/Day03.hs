{-# LANGUAGE TupleSections #-}
module Day03 where

import           Advent.Grid     (readGridFromDay)
import qualified Advent.Grid     as G
import           Advent.Modulo
import           Advent.Vector   (Vector (Vector))
import           Data.List       (unfoldr)
import           Data.Map.Strict ((!?))
import           Data.Semigroup

{- |
Represents the rectangular Grid that we 
recieve as input.
-}
type Grid = G.Grid Int Int

{- | 
Represents the cylinder that we get by wrapping the Grid input
around it's self along the X-axis.  

Positions on the grid are given by :
* Integers Mod N, where N is the width of the input grid;
* Integers.

The use of Sum in the second position argument ensures that both co-ordinate
types describing position are Semi Groups, so we can apply translations.
-}
type CylinderGrid = G.Grid Mod (Sum Int)

toCylinderGrid :: Grid -> (Int, CylinderGrid)
toCylinderGrid grid =
  let
    (maxX, _) = G.maxPos grid
    n = maxX + 1
  in (n, G.map (Mod n) Sum grid)

{- |
Returns the string representing positions in the grid visited by repeated
  translations by a shift.  
-}
path :: (Ord a, Ord b, Semigroup a, Semigroup b) => G.Grid a b -> Vector a b -> G.Position a b  -> String
path grid shift  = unfoldr (nextPosition shift grid)
  where nextPosition shift grid pos = (,pos <> shift) <$> grid !? pos

treeCount :: (Ord b, Semigroup b, Num b) => G.Grid Mod b -> Vector Int b -> Int -> Int
treeCount cylinder (Vector x y) n  = 
  length $ 
  filter ('#' ==) $ 
  path cylinder (Vector (Mod n x) y) (Vector (Mod n 0) 0) 

partOne :: G.Grid Int Int -> Int
partOne grid  =
  let (w, cylinder) = toCylinderGrid grid
  in treeCount cylinder (Vector 3 1) w

partTwo :: G.Grid Int Int -> Int
partTwo grid =
    let (w, cylinder) = toCylinderGrid grid
    in product [
        treeCount cylinder (Vector 1 1) w
      , treeCount cylinder (Vector 3 1) w
      , treeCount cylinder (Vector 5 1) w
      , treeCount cylinder (Vector 7 1) w
      , treeCount cylinder (Vector 1 2) w
      ]

main :: IO ()
main = do
  input <- readGridFromDay 3
  putStrLn $ "Part One: " ++ show ( partOne input)
  putStrLn $ "Part Two: " ++ show ( partTwo input)