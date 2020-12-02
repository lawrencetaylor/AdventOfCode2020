{-# LANGUAGE FlexibleContexts #-}

module Advent.Parsing(
  pInt
  , pNat
  , pDay
  , pDayLines) where

import           Data.Char                     (digitToInt)
import           Text.Parsec                   (digit,many1, parse)
import           Text.Parsec.Char              (char)
import           Text.Parsec.Combinator        (option)
import           Text.ParserCombinators.Parsec (Parser)

dayFileName :: Int -> String
dayFileName day
  | day < 10 = "inputs/day0" ++ show day
  | otherwise = "inputs/day" ++ show day

readDay :: Int -> IO String
readDay day = readFile $ dayFileName day

readDayLines :: Int -> IO [String]
readDayLines = fmap lines . readDay

parseWith :: Parser a -> String -> a
parseWith p str = 
  case parse p [] str of
    Right a -> a
    Left e -> error $ show e

{- Parses the contents of the input file for
a given day, using the specified parser on the entire
contents of the file. -}
pDay :: Int -> Parser a -> IO a
pDay day parser = do 
  dayContents <- readDay day 
  return $ parseWith parser dayContents

{- Parses the contents of the input file for
a given day, using the specified parser on each of the 
lines. -}
pDayLines :: Int -> Parser a -> IO [a]
pDayLines day parser = do
  lines <- readDayLines day
  let parsedLines = parseWith parser <$> lines
  return parsedLines

pDigits :: Parser [Int]
pDigits = (fmap . fmap) digitToInt $ many1 digit

arrayToInt :: [Int] -> Int
arrayToInt = foldl (\a b -> b + 10*a) 0

{- Parses a natural number to integer. -}
pNat :: Parser Int
pNat = fmap arrayToInt $ pDigits


{- Parses a positive or negative integer. -}
pInt :: Parser Int
pInt = do
  multiplier <- toInt <$> (option '+' $ char '-')
  value <- fmap arrayToInt $ pDigits
  return $ multiplier * value
  where
    toInt :: Char -> Int
    toInt '+' = 1
    toInt '-' = -1