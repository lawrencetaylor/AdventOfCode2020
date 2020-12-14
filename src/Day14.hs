module Day14 where

import           Advent.Parsing                (pDayLines, pInt, (>>.))
import           Data.Bits                     (bit, clearBit, setBit)
import           Data.Map                      (Map, empty, insert, toList)
import           Text.ParserCombinators.Parsec (Parser, alphaNum, many, string,
                                                try, (<|>))

data MaskValue = Value Int | X deriving(Show)
type Mask = [MaskValue]
type Address = Int

data Action =
  SetMask Mask
  | SetAddress Address Int
  deriving (Show)

-- Parsing

readMask :: String -> Mask
readMask s = map <$> reverse s
  where
    map 'X' = X
    map '0' = Value 0
    map '1' = Value 1

pSetMask :: Parser Action
pSetMask = do
  maskStr <- try (string "mask = ") >>. many alphaNum
  return $ SetMask (readMask maskStr)

pSetAddress :: Parser Action
pSetAddress = do
  address <- try (string "mem[") >>. pInt
  value <- string "] = " >>. pInt
  return $ SetAddress address value

pAction :: Parser Action
pAction = pSetAddress <|> pSetMask

-- Solution

{-|
Applies the mask by Clearing/Setting the relevant bit of the input
depending on whether the mask value is 1 or 0.
-}
applyMask1 :: Mask -> Int -> Int
applyMask1 m i = foldr map i (zip [0..] m)
  where
    map (i, Value 0) j = clearBit j i
    map (i, Value 1) j = setBit j i
    map (_, X) j       = j

{-|
Applies the mask by 
* (Value 1) sets the relevant bit of the input to 1;
* (Value 0) has no effect;
* X considers both the possibilities that the relevant bit
of the input is both set and cleared.
-}
applyMask2 :: Mask -> Int -> [Int]
applyMask2 m i = foldr map [i] (zip [0..] m)
  where
    map (_, Value 0) js = js
    map (i, Value 1) js = flip setBit i <$> js
    map (i, X) js       = (flip setBit i <$> js) ++ (flip clearBit i <$> js)

data Machine = Machine {
    mask   :: Mask
  , memory :: Map Int Int
  }
  deriving (Show)

initialMachine :: Machine
initialMachine = Machine [] empty

{-|
Applies the mask (using applyMask1) to the value being
assigned to each memory address.
-}
apply1 :: Action -> Machine -> Machine
apply1 (SetMask m) mac = mac { mask = m }
apply1 (SetAddress a v) mac = mac { memory = newMemory }
  where
    mem = memory mac
    v' = applyMask1 (mask mac) v
    newMemory = insert a v' mem

{-|
Applies the mask (using applyMask2) to the address being
assigned to.  This results in the value being assigned
to multiple addresses.
-}
apply2 :: Action -> Machine -> Machine
apply2 (SetMask m) mac = mac { mask = m }
apply2 (SetAddress a v) mac = mac { memory = newMem }
  where
    mem = memory mac
    addresses = applyMask2 (mask mac) a
    newMem = foldr (`insert` v) mem addresses

getMemorySum :: (Action -> Machine -> Machine) -> [Action] -> Int
getMemorySum folder actions =
  getSum $
  foldr folder initialMachine (reverse actions)
  where
    getSum  = sum . fmap snd . toList . memory

partOne :: [Action] -> Int
partOne = getMemorySum apply1

partTwo:: [Action] -> Int
partTwo = getMemorySum apply2

main :: IO ()
main = do
  input <- pDayLines 14 pAction
  putStrLn $ "Part One: "  ++ show (partOne input)
  putStrLn $ "Part Two: "  ++ show (partTwo input)