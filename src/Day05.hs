module Day05 where

import Advent.Parsing
import Data.List (sort)

{-| 
Specifies whether to continue searching an interval in the Upper
or Lower half when executing a binary search.
-}
data LookDirection = Upper | Lower deriving (Eq, Show)

chop :: Integral a => (a, a) -> LookDirection -> (a, a)
chop (l, h) x = 
  let 
    mid = (l + h) `div` 2
  in case x of
    Lower -> (l, mid)
    Upper -> (mid + 1,  h)

findSeat :: String -> Int
findSeat code = 8*row+col
  where
    (rowCode, colCode) = splitAt 7 code

    {-| 
    After performing the binary search both the 
    upper and lower bounds of the interval will be 
    the same.
    -}
    find :: (Int,Int) -> [LookDirection] -> Int
    find bounds = fst . foldl chop bounds 

    row = 
      find (0, 127) $
      (\c -> if c == 'F' then Lower else Upper) <$> 
      rowCode
    
    col = 
      find (0, 7) $
      (\c -> if c == 'L' then Lower else Upper) <$> 
      colCode

partOne :: (Foldable t, Functor t) => t String -> Int
partOne input = 
  maximum $ findSeat <$> input

partTwo :: [String] -> Int
partTwo input = 
  findSeatId $ 
  sort $ findSeat <$> input

findSeatId :: [Int] -> Int
findSeatId (x:y:z:xs) 
  -- (a-1, a, a+1)
  | x + y + z == 3*y = findSeatId (y:z:xs)
  -- (a-1, a, a+2) 
  | x + y + z == 3*y+1 = y + 1

main :: IO ()
main = do
  input <- readDayLines 5

  putStrLn $ "Part One: " ++ show (partOne input)
  putStrLn $ "Part Two: " ++ show (partTwo input)