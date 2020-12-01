module Advent.List(
  distinctN) where

import           Data.List

{- Returns a list of the n-tuples (each as a list) you
can form from an initial list.
-}
distinctN :: (Eq t, Num t) => t -> [a] -> [[a]]
distinctN 1 xs = [ [x] | x <- xs ]
distinctN n xs = do
  (x : rest) <- tails xs
  next <- distinctN (n-1) rest
  return (x : next)