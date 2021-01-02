module Day24 where

import           Advent.Parsing                  (pDayLines)
import           Advent.Vector                   (Vector (Vector))
import qualified Advent.Vector                   as Vector
import           Control.Applicative.Combinators (choice)
import           Data.Semigroup                  (Sum (getSum))
import           Data.Set                        (Set)
import qualified Data.Set                        as Set
import           Text.ParserCombinators.Parsec   (Parser, char, many1, string,
                                                  try)

data Direction =  E | SE | SW | W | NW | NE  deriving (Show)

{-| A Hexagon is represented by the coordinates of its centre. -}
type Hexagon = Vector (Sum Int) (Sum Int)

type BlackTiles = Set Hexagon

-- Parsing

pDirection :: Parser Direction
pDirection = choice [
    SE <$ try (string "se")
  , E <$ char 'e'
  , SW <$ try (string "sw")
  , W <$ char 'w'
  , NW <$ try (string "nw")
  , NE <$ try (string "ne")
  ]

-- Solution

move :: Direction -> Hexagon -> Hexagon
move E  = (<>) (Vector 2 0)
move SE = (<>) (Vector 1 (-3))
move SW = (<>) (Vector (-1) (-3))
move W  = (<>) (Vector (-2) 0)
move NW = (<>) (Vector (-1) 3)
move NE = (<>) (Vector 1 3)

adjacentTo :: Hexagon -> Set Hexagon
adjacentTo location =
  Set.fromList $
  (`move` location) <$>
  [E, SE, SW, W, NW, NE]

adjacentBlackCount :: BlackTiles -> Hexagon -> Int
adjacentBlackCount blacks  =
  length .
  Set.filter (`Set.member` blacks) .
  adjacentTo

flipAll :: BlackTiles -> BlackTiles
flipAll blacks = remainBlack `Set.union` whiteToBlack
  where
    remainBlack =
      Set.filter (\l -> adjacentBlackCount blacks l `elem` [1,2])
      blacks

    whiteToBlack =
      Set.filter (\l -> adjacentBlackCount blacks l == 2) $
      Set.filter (not . (`Set.member` blacks)) $
      Set.unions $
      Set.map adjacentTo blacks

initialBlackTiles :: [[Direction]] -> BlackTiles
initialBlackTiles directions =
  foldl flipTile Set.empty (goToHexFrom (Vector 0 0) <$> directions)
  where
    goToHexFrom :: Hexagon -> [Direction] -> Hexagon
    goToHexFrom = foldr move

    flipTile :: BlackTiles -> Hexagon -> BlackTiles
    flipTile blacks location =
      if location `Set.member` blacks
      then location `Set.delete` blacks
      else location `Set.insert` blacks

partOne :: [[Direction]] -> Int
partOne directions = length $ initialBlackTiles directions

partTwo :: [[Direction]] -> Int
partTwo directions =
  length $
  iterate flipAll (initialBlackTiles directions) !! 100

main :: IO ()
main = do
  directions <- pDayLines 24 (many1 pDirection)
  putStrLn $ "Part One: " ++ show (partOne directions)
  putStrLn $ "Part Two: " ++ show (partTwo directions)