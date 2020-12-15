module Day15 where

import           Data.Foldable (Foldable (foldl'))
import           Data.Map      (Map, fromList, insert, (!?))

data State = State {
    previousValue   :: Int
  , currentPosition :: Int
  {- 
  The use of ! here is important.  It means that
  updates to the structure are not lazily evaluated, which 
  can build up memory on the stack.
  https://stackoverflow.com/a/50516493/2382536
  -}
  , seen            :: !(Map Int Int) 
  }
  deriving(Show)

initial :: [Int] -> State
initial inputs = State lastValue i seen
  where
    ((lastValue, i):rest) = reverse (zip inputs [0..])
    seen = fromList rest

next :: State -> State
next (State  previous current seen) = State newValue nextPosition newSeen
  where
    nextPosition = current + 1
    newValue  = maybe 0 (current -) (seen !? previous)
    newSeen = insert previous current seen

{-|
NOTE: the foldl' (as opposed to foldl) is really important.  
Without it the number of unevaluated expressions on the stack grows.
See: https://wiki.haskell.org/Foldr_Foldl_Foldl'
-}
findIndex :: Int -> [Int] ->  Int
findIndex i input = previousValue $ foldl' (\s _  -> next s) seed [0..i-1 - inputLength]
  where
    seed = initial input
    inputLength = length input


partOne :: [Int] -> Int
partOne = findIndex 2020 

partTwo :: [Int] -> Int
partTwo = findIndex 30000000 

main :: IO ()
main = do
  let input =  [0,8,15,2,12,1,4]
  putStrLn $ "Part One: "  ++ show (partOne input)
  putStrLn $ "Part Two: "  ++ show (partTwo input)
