module Day11 where

import           Advent.Grid     (Grid, Position (..), neighbours,
                                  readGridFromDay)
import qualified Advent.Grid     as G
import           Advent.Vector   (Vector)
import qualified Data.Map.Strict as M
import           Data.Maybe      (mapMaybe)
import           Data.Semigroup  (Sum (Sum))

{-|
This is certainly not a fast solution!
-}

type Seats = Grid (Sum Int) (Sum Int)

type Direction = Vector (Sum Int) (Sum Int)

type Seat = Position (Sum Int) (Sum Int)

type GetNeighbours = Seats -> Seat -> [(Seat, Char)]

{-|
Applies the rules to determine whether a seat moves
to being occupied or not.
-}
changeOccupancy ::  Int -> [(Seat, Char)]  -> Char ->  Char
changeOccupancy limit neighbours  v = inner neighbours
  where
    inner :: [(Seat, Char)] -> Char
    inner  neighbours
      | (v == 'L') && all (\p -> snd p /= '#') neighbours = '#'
      | (v == '#') && (>= limit) (length $ filter ((== '#') . snd ) neighbours) = 'L'
      | otherwise = v

{-|
Evolves the seating arrangement from a given starting point.
-}
seatEvolution ::
  Int -- ^ If an occupied seat has more than this number of "neighbouring" occupied seats it will become empty
  -> GetNeighbours -- ^ A function that determines the "neighbouring seats"
  -> Seats -> [Seats]
seatEvolution limit getNeighbours  = iterate (allChange limit getNeighbours )
  where
    allChange :: Int -> GetNeighbours -> Seats -> Seats
    allChange limit getNeighbours seats = M.mapWithKey change seats
      where
        change :: Seat -> Char -> Char
        change seat = changeOccupancy limit (getNeighbours seats seat)

{-|
Returns the stable seating arrangement that is the
limit of the evolution.
-}
stableOccupancy :: [Seats] -> Seats
stableOccupancy (x:y:xs)
  | x /= y = stableOccupancy (y:xs)
  | x == y = x

countOccupied :: Seats -> Int
countOccupied = length . M.filter (== '#')

stableSeatCount ::  Int -> GetNeighbours -> Seats -> Int
stableSeatCount limit getNeighbours seats = countOccupied final
  where
    seatSequence = seatEvolution limit getNeighbours seats
    final = stableOccupancy seatSequence

{-|
Returns all those seats that are "viewable" from a given seat.
i.e. those that do not have seats obscuring them along the 8 directions
of the compass.
-}
viewableNeighbours :: GetNeighbours
viewableNeighbours grid pos = mapMaybe (moveD grid pos) G.kingMoves
  where
    moveD :: Seats -> Seat -> Direction -> Maybe (Seat, Char)
    moveD seats seat d =
      let next  = (d <> seat)
      in
        case seats M.!? next of
          Nothing  -> Nothing
          Just '.' -> moveD seats next d
          Just 'L' -> Just (seat, 'L')
          Just '#' -> Just (seat, '#')

partOne :: Seats -> Int
partOne = stableSeatCount 4 neighbours

partTwo :: Seats -> Int
partTwo = stableSeatCount 5 viewableNeighbours

main :: IO ()
main = do
  grid <- readGridFromDay 11
  putStrLn $ "Part One: "  ++ show (partOne $ G.map Sum Sum grid)
  putStrLn $ "Part Two: "  ++ show (partTwo $ G.map Sum Sum grid)