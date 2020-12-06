module Day06 where

import Advent.Parsing (readDayLines)
import Data.Set (Set, insert, empty, union)
import Data.Set.Internal (intersection)

type Person = String
type Group = [Person]
type YesAnswers = Set Char

readGroup :: [Person] -> [Group] -> [String] -> [Group]
readGroup currentGroup groups [] = reverse (currentGroup:groups)
readGroup currentGroup groups ("":xs) = readGroup [] (reverse currentGroup:groups) xs
readGroup currentGroup groups (x:xs) = readGroup (x:currentGroup) groups xs

countYes :: (YesAnswers -> YesAnswers -> YesAnswers) ->  [Group] -> Int
countYes combiner = sum . fmap groupCount
  where 
    personCount = foldr insert empty
    groupCount = length . foldl1 combiner . fmap personCount 

partOne :: [Group] -> Int
partOne = countYes union

partTwo :: [Group] -> Int
partTwo = countYes intersection

main :: IO ()
main = do
  groups <- readGroup  [] [] <$> readDayLines 6
  putStrLn $ "Part One: " ++ show (partOne groups)
  putStrLn $ "Part Two: " ++ show (partTwo groups)