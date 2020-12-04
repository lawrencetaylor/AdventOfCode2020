{-# LANGUAGE TupleSections #-}
module Day04 where

import           Text.ParserCombinators.Parsec (Parser)

import           Advent.Parsing                (pInt, parseWith, readDay, (.>>),
                                                (.>>.))
import           Control.Applicative           (Alternative ((<|>)))
import           Control.Monad                 (replicateM, replicateM_)
import           Data.Functor                  (void)
import           Data.List                     (find)
import           Data.Map                      (Map, empty, insert, keys, (!?))
import           Data.Maybe                    (isJust)
import           Data.Tuple                    (swap)
import           Text.Parsec                   (char, choice, eof, many1,
                                                newline, noneOf, parse, try)
import           Text.Parsec.Char              (string)

-- Model

type Key = String
type Value = String
type PassportItem =  (Key, String)
type Passport = Map String String

{-| 
Represents a "unit of input" that we read from 
the input file.
-}
data Token =
  Item PassportItem
  | ItemSeperator
  | PassportSeperator
  deriving (Eq, Show)

-- Parsing

pPassportItem :: Parser PassportItem
pPassportItem = choice [
    pItem "byr" pValue
    , pItem "iyr" pValue
    , pItem "eyr" pValue
    , pItem "hgt" pValue
    , pItem "hcl" pValue
    , pItem "ecl" pValue
    , pItem "pid" pValue
    , pItem "cid" pValue
  ]

  where 
    pValue :: Parser String
    pValue = many1 $ noneOf [' ', '\n']

    pItem :: String -> Parser String -> Parser PassportItem
    pItem itemCode parser =  
      (itemCode, ) <$> 
      try (string itemCode >> char ':' >> parser)

pItemSeperator :: Parser ()
pItemSeperator = void (char ' ' <|> newline)

pPassportSeperator :: Parser ()
pPassportSeperator = replicateM_ 2 newline

pParsingToken :: Parser PassportItem -> Parser Token
pParsingToken passPortItem = choice [
    Item <$> try passPortItem
    , PassportSeperator <$ try pPassportSeperator
    , ItemSeperator <$ try pItemSeperator
  ]

readPassports :: [Token] -> Passport -> [Passport] -> [Passport]
readPassports [] current existing = reverse (current : existing)
readPassports (Item (key ,value):xs) current existing = readPassports xs (insert key value current ) existing
readPassports (ItemSeperator:xs) current existing = readPassports xs current existing
readPassports (PassportSeperator:xs) current existing = readPassports xs empty (current : existing)

-- 

hasMandatoryAttributes :: Passport -> Bool
hasMandatoryAttributes values =
  let requiredKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
  in  isJust $ sequenceA $ (\ k -> find (== k) (keys values)) <$>  requiredKeys


between :: Int -> Int -> Int -> Bool
between lower upper x = x >= lower && x <= upper

pHeight :: Parser (String, Int)
pHeight = 
  swap <$> 
  pInt .>>. 
  (try (string "cm") <|> try (string "in")) 
  .>> eof

checkHeight :: String -> Int -> Bool
checkHeight "cm" = between 150 193
checkHeight "in" = between 59 76

pHairColor :: Parser String
pHairColor = do
  hash <- char '#'
  color <- replicateM 6 pColourChar .>> eof
  return $ hash : color
  where

    pChar = choice [char c | c <- ['a'..'f'] ]
    pNum = choice [char c | c <- ['0'..'9'] ]
    pColourChar = pChar <|> pNum

pEyeColor :: Parser String
pEyeColor =
  choice [
      try $ string "amb"
    , try $ string "blu"
    , try $ string "brn"
    , try $ string "gry"
    , try $ string "grn"
    , try $ string "hzl"
    , try $ string "oth"
  ]

pPassportId :: Parser String
pPassportId = replicateM 9 $ choice [char c | c <- ['0'..'9'] ] 

{-| 
Validates the specified string by:

1. Checking it can be parsed using the specified parser;
2. Verifying the result passes the specified predicate.

-}
isValid :: Parser a -> (a -> Bool) -> String -> Bool
isValid parser f str = Just True == (f <$> pToMaybe parser str)
  where

    {-|
    If the parser fails, returns Nothing.
    Otherwise if x is the result of the parser Just x is returned.
    -}
    pToMaybe :: Parser a -> String -> Maybe a
    pToMaybe parser = either (const Nothing) Just . parse parser ""

check :: String -> (String -> Bool) -> Passport -> Bool
check key f passport = Just True == (f <$> passport !? key)

partOne :: String -> [Passport]
partOne input = 
  let 
    tokens = parseWith (many1 $ pParsingToken pPassportItem) input
    passports = readPassports tokens empty []
  in filter hasMandatoryAttributes passports

partTwo :: [Passport] -> [Passport]
partTwo = filter satisfiesAllRules
  where 
    satisfiesAllRules :: Passport -> Bool
    satisfiesAllRules p = 
      all (\f -> f p) 
      [
        check "byr" $ isValid pInt (between 1920 2002)
      , check "iyr" $ isValid pInt (between 2010 2020)
      , check "eyr" $ isValid pInt (between 2020 2030)
      , check "hgt" $ isValid pHeight (uncurry checkHeight)
      , check "hcl" $ isValid pHairColor (const True)
      , check "ecl" $ isValid pEyeColor (const True)
      , check "pid" $ isValid pPassportId (const True)
      ]

main :: IO ()
main = do
  input <- readDay 4
  let validPassports1 = partOne input
  let validPassports2 = partTwo validPassports1
  
  putStrLn $ "Part One: " ++ show (length validPassports1)
  putStrLn $ "Part Two: " ++ show (length validPassports2)
