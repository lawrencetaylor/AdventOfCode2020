{-# LANGUAGE FlexibleContexts #-}

module Advent.Parsing(
  pInt
  , pDigits
  , pNat
  , pDay
  , pDayLines
  , readDay
  , readDayLines
  , pDaySepBy
  , parseWith
  , tryParse
  , (.>>)
  , (.>>.)
  , (>>.)) where

import           Data.Char                     (digitToInt)
import           Text.Parsec                   (getParserState, digit, many1, parse, sepBy)
import           Text.Parsec.Char              (char)
import           Text.Parsec.Combinator        (option)
import           Text.ParserCombinators.Parsec (State(stateInput), Parser)
import Control.Applicative

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
    Left e  -> error $ show e

tryParse :: Parser a -> String -> Maybe a
tryParse p str =
  case parse p [] str of
    Right a -> Just a
    Left e  -> Nothing

pUnprocessed :: Parser String
pUnprocessed= do
  s <- getParserState
  let out = stateInput s
  return out
    

{- Parses the contents of the input file for
a given day, using the specified parser on the entire
contents of the file. -}
pDay :: Int -> Parser a -> IO a
pDay day parser = do
  dayContents <- readDay day
  return $ parseWith parser dayContents

pDaySepBy :: Int -> Parser a ->  Parser ()  -> IO [a]
pDaySepBy day parser seperator = do
  dayContents <- readDay day
  return $ parseWith (sepBy parser seperator) dayContents

{- Parses the contents of the input file for
a given day, using the specified parser on each of the
lines. -}
pDayLines :: Int -> Parser a -> IO [a]
pDayLines day parser = do
  lines <- readDayLines day
  let parsedLines = parseWith parser <$> lines
  return parsedLines

pDigits :: Parser [Int]
pDigits = fmap digitToInt <$> many1 digit

arrayToInt :: [Int] -> Int
arrayToInt = foldl (\a b -> b + 10*a) 0

{- Parses a natural number to integer. -}
pNat :: Parser Int
pNat = arrayToInt <$> pDigits

{- Parses a positive or negative integer. -}
pInt :: Parser Int
pInt = do
  multiplier <- toInt <$> option '+' (char '-' <|> char '+')
  value <- arrayToInt <$> pDigits
  return $ multiplier * value
  where
    toInt :: Char -> Int
    toInt '+' = 1
    toInt '-' = -1

(.>>.) :: Parser a -> Parser b -> Parser (a, b)
(.>>.) pA  pB = do
  a <- pA
  b <- pB
  return (a, b)

(.>>) :: Parser a -> Parser b -> Parser a
(.>>) a b = fst <$> a .>>. b

(>>.) :: Parser a -> Parser b -> Parser b
(>>.) a b = snd <$> a .>>. b




