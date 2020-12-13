{-# LANGUAGE TupleSections #-}
module Day13 where

import qualified Advent.Modulo                      as M (Mod (..), createMod,
                                                          solveChineseRemainder,
                                                          value)
import           Advent.Parsing                     (pInt, parseWith,
                                                     readDayLines, (>>.))
import           Data.List                          (sortOn)
import           Data.Maybe                         (catMaybes)
import           Text.ParserCombinators.Parsec      (Parser, sepBy, (<|>))
import           Text.ParserCombinators.Parsec.Char (char)



type Time = Integer
type Bus = Maybe Time
type TimeTable = [Bus]

-- Parsing

pBus :: Parser Bus
pBus =
  Nothing <$ char 'x'
  <|>
  Just . toInteger <$> pInt

pTimeTable :: Parser TimeTable
pTimeTable = sepBy pBus (char ',')

-- Solution

partOne :: TimeTable -> Time -> Integer
partOne timeTable time = time' * p
  where
    (time', p) =
      head $
      sortOn fst $
      (\p -> (M.value $ M.createMod p (-time), p)) <$>
      catMaybes timeTable

partTwo :: TimeTable -> Integer
partTwo timetable = M.solveChineseRemainder constraints
  where
    -- Create a list [0, -1, -2, ... ]
    offsets = iterate (\x -> x - 1) 0

    toMaybeTuple :: (Maybe a, b) -> Maybe (a, b)
    toMaybeTuple (x, b) = fmap (, b) x

    constraints =
      fmap (uncurry M.Mod)  -- [ Mod prime offset ]
      $ catMaybes $         -- [ (prime, offset)]
      toMaybeTuple <$>      -- [ Maybe (prime, offset) ]
      zip timetable offsets -- [ (Maybe prime, offset) ]

main :: IO ()
main = do
  lines <- readDayLines 13
  let time = parseWith (toInteger <$> pInt) (head lines)
  let timetable = parseWith pTimeTable (lines !! 1)
  putStrLn $ "Part One: "  ++ show (partOne timetable time)
  putStrLn $ "Part two: "  ++ show (partTwo timetable)
