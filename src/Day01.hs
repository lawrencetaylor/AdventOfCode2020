module Day01 where

import           Advent.List    (distinctN)
import           Advent.Parsing (pDayLines, pInt)

reportProduct :: (Eq a, Eq t, Num a, Num t) => t -> [a] -> a
reportProduct n  expenses =
  product $
  head $
  filter ((==) 2020 .  sum) $
  distinctN n expenses

partOne :: (Eq a, Num a) => [a] -> a
partOne = reportProduct 2

partTwo :: (Eq a, Num a) => [a] -> a
partTwo = reportProduct 3

main :: IO ()
main = do
  input <- pDayLines 1 pInt
  putStrLn $ "Part One: " ++ show ( partOne input)
  putStrLn $ "Part Two: " ++ show ( partTwo input)