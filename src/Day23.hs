{-# LANGUAGE NumericUnderscores #-}

module Day23 where

import           Advent.Monad   (applyM, iterateM)
import           Advent.Parsing (pDigits, parseWith)
import           Data.Array.IO
import qualified Data.Array.ST  as Array

type Neighbours = IOArray Int Int
type GetDestination = [Int] -> Int -> Int

destination :: Int -> GetDestination
destination n picked x
  | y `elem` picked = destination n picked y
  | otherwise = y
    where
      y = previous x

      previous 1 = n
      previous a = a - 1

{-|
WARNING:  This function mutates the array of neighbours
in place.

It returns the new "current" position from which the next move
starts from.
-}
move :: GetDestination -> Neighbours -> Int -> IO Int
move getDestination neighbours c  = do
    (l, u) <- getBounds neighbours

    p1 <- Array.readArray neighbours  c
    p2 <- Array.readArray neighbours p1
    p3 <- Array.readArray neighbours p2

    neighbours' <- Array.readArray neighbours p3

    let dest = getDestination [p1,p2,p3] c
    dn <- Array.readArray neighbours dest
    _ <- writeAll neighbours [(c, neighbours'), (dest, p1), (p3, dn)]

    return neighbours'

{-| 
Helper function to do a bulk update on an IOArray
-}
writeAll :: IOArray Int Int -> [(Int, Int)] -> IO ()
writeAll arr x = (() <$) $ sequenceA $ uncurry (Array.writeArray arr) <$> x

neighboursArray :: [Int] -> IO Neighbours
neighboursArray input = do
  let n = length input
      positions = take n $ zip input (tail (input ++ input))
  arr <- Array.newArray (1,length input) 0 :: IO (IOArray Int Int)
  writeAll arr positions
  return arr

{-|
Converts an IOArray of Neighbours to an integer array from
an initial "seed".
E.g. (toIntArray 1 [5,4,2,1,3]) creates the array representing
the neighbours starting from 1: [5,3,2,4,1]
-}
toIntArray :: Int -> Neighbours -> IO [Int]
toIntArray j arr = do
  (l, u ) <- getBounds arr
  let
    n = u - l
  take n <$> iterateM n (readArray arr ) j

partOne :: [Int] -> IO String
partOne input@(p0:_) = do
  neighbours <- neighboursArray input
  let n = length input
      f = move (destination n) neighbours
  _ <- applyM 100 f p0
  x <- take n <$> toIntArray 1 neighbours
  return $ concatMap show x

partTwo :: [Int] -> IO Int
partTwo input@(p0:_) = do
  let input' = input ++ [(length input + 1)..1_000_000]
  neighbours <- neighboursArray input'
  let f = move (destination 1_000_000) neighbours
  _ <- applyM 10_000_000 f p0
  product . take 2 <$> toIntArray 1 neighbours

main :: IO ()
main = do
  let
    input@(p0:_) = parseWith pDigits "186524973"-- "389125467"
  partOne input >>= (\p1 -> putStrLn $ "Part One: " ++ p1)
  partTwo input >>= (\p2 -> putStrLn $ "Part Two: " ++ show p2)
