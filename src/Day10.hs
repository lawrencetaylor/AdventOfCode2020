module Day10 where

import Advent.Parsing (pInt, pDayLines)

import Data.List

addMainsAndMine :: [Int] -> [Int]
addMainsAndMine input = input ++ [0, maximum input + 3]

differences :: [Int] -> [Int]
differences (x:y:xs) = y-x:differences(y:xs)
differences _ = []

partOne :: [Int] -> Int
partOne input = diff1 * diff3
  where
    diffs = differences input
    diff1 = length $ filter (== 1) diffs
    diff3 = length $ filter (== 3) diffs

{-

For Part 2: 

Let's build this up for [0,1,4,5,6,7,10,11,12,15,16,19,22].

We are going to start at the end, and build up an array pathCount[i] where

pathCount[i] = # of possible paths from  "i+1th from the end" to the end.

pathCount[0]
========
[22,19,16,15,12,11,10,7,6,5,4,1,0]
     ^
Where can I go from 19?  Only to 22.  pathCount [0] = 1

pathCount[1]
========
[22,19,16,15,12,11,10,7,6,5,4,1,0]
       ^
Where can I go from 16?  Only to 19.  pathCount [1] = 1

pathCount[2]
========
[22,19,16,15,12,11,10,7,6,5,4,1,0]
           ^
Where can I go from 15?  Only to 16.  pathCount [2] = 1

pathCount[3]
========
[22,19,16,15,12,11,10,7,6,5,4,1,0]
              ^
Where can I go from 12?  Only to 15.  pathCount [3] = 1

pathCount[4]
========
[22,19,16,15,12,11,10,7,6,5,4,1,0]
                 ^
Where can I go from 11?  Only to 12.  pathCount [4] = 1

pathCount[5]
========
[22,19,16,15,12,11,10,7,6,5,4,1,0]
                 ^
Where can I go from 11?  Only to 12.  pathCount [5] = 1

paths[6]
========
[22,19,16,15,12,11,10,7,6,5,4,1,0]
                    ^
Where can I go from 10?  More intesting!
- Go to 11.  From there we know there is 1 path home pathCount[4]
- Go to 12.  From there we know there is 1 path home pathCount[3] => pathCount[6] = 2

pathCount[7]
========
[22,19,16,15,12,11,10,7,6,5,4,1,0]
                      ^
Where can I go from 7?   Only to 10  pathCount[7] = 2

pathCount[8]
========
[22,19,16,15,12,11,10,7,6,5,4,1,0]
                        ^
Where can I go from 6?   Only to 7  pathCount[8] = 2

pathCount[9]
========
[22,19,16,15,12,11,10,7,6,5,4,1,0]
                          ^
Where can I go from 5? 
- Go to 6.  From there we know there is 2 paths home pathCount[8]
- Go to 7.  From there we know there is 2 paths home pathCount[7] => pathCount[9] = 4

pathCount[10]
========
[22,19,16,15,12,11,10,7,6,5,4,1,0]
                            ^
Where can I go from 4? 
- Go to 5.  From there we know there is 4 paths home pathCount[9]
- Go to 6.  From there we know there is 2 paths home pathCount[8]
- Go to 7.  From there we know there is 2 paths home pathCount[7] => pathCount[10] = 8

pathCount[11]
========
[22,19,16,15,12,11,10,7,6,5,4,1,0]
                              ^
Where can I go from 1?  Only to 4  pathCount[11] = 8

pathCount[12]
========
[22,19,16,15,12,11,10,7,6,5,4,1,0]
                                ^
Where can I go from 0?  Only to 1  pathCount[12] = 8

In general:

pathCount[i] = sum_k pathCount[i-k] : k = 1,2,3 where input[i+k] is within 3 of input[i]

-}
pathCount :: [Int] -> Int -> [Int] -> [Int]
pathCount input i paths = pathCountFrom_i : paths
  where
    {-  The indices are slightly different above because we pushing the newly found pathCount[i] on 
        to the beginning of the accumulated list each time.
    -}
    pathsFromValidNextSeps = [ paths !!  (k-1) | k <- [1..3], i-k >= 0, (input!!(i-k) - input !! i) <= 3 ]
    pathCountFrom_i =  sum pathsFromValidNextSeps


partTwo :: [Int] -> Int
partTwo input = head $ paths (n-1)
  where
    reversed = reverse input
    n = length input

    paths 0 = [1]
    paths i = pathCount reversed i (paths (i-1))

main :: IO ()
main = do
  input <- pDayLines 10 pInt
  let adapaters  = sort $ addMainsAndMine input
  putStrLn $ "Part One: " ++ show (partOne adapaters)
  putStrLn $ "Part Two: " ++ show (partTwo adapaters)