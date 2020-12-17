module Day17 where

import           Advent.Grid   (Grid, gridString, printGrid, readGridFromDay)
import           Advent.Vector
import qualified Data.Map      as M
import           Data.Maybe    (fromMaybe)
import           Data.Set      (Set, member)
import qualified Data.Set      as Set

type Coord = [Int]

type Active = Set Coord

findNeighbours :: [Int] -> [[Int]]
findNeighbours  = traverse (\i -> [i-1, i, i+1])

{-|
Note that the rules are equivalent to:
* Count the number of active points in the whole 3 x 3 cube of points (including the centre)
* If this count is equal to 3, the central points is active
* If this count is equal to 4, and the central point was active, it remains active
* Otherwise it is not active.
-}
isActiveNext :: Active -> Coord -> Bool
isActiveNext active coord =
  let
    activeNeighbours = length . filter (`member` active) . findNeighbours
    activeCount = activeNeighbours coord

  in
    case (coord `member` active, activeCount) of
    (_, 3)    -> True
    (True, 4) -> True
    _         -> False

evolution :: Active -> [Active]
evolution  = iterate evolve
  where
    evolve :: Active -> Active
    evolve active
      = Set.filter (isActiveNext active)
      $ Set.fromList
      $ concatMap findNeighbours
      $ Set.toList active

partOne :: Grid Int Int -> Int
partOne grid = length $ evolution initial !! 6
  where
    initial =  Set.fromList $ (\ (Vector a b, v) -> [a, b, 0]) <$> M.toList (M.filter (== '#') grid)

partTwo :: Grid Int Int -> Int
partTwo grid = length $ evolution initial !! 6
  where
    initial = Set.fromList $ (\ (Vector a b, v) -> [a, b, 0, 0]) <$> M.toList (M.filter (== '#') grid)

main :: IO ()
main = do
  grid <- readGridFromDay 17
  putStrLn $ "Part One: " ++ show ( partOne grid)
  putStrLn $ "Part Two: " ++ show ( partTwo grid)
