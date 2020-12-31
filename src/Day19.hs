module Day19 where

import           Advent.Parsing                (pInt, parseWith, readDayLines,
                                                (.>>), (.>>.), (>>.))
import           Control.Applicative           (Alternative ((<|>)))
import           Control.Monad                 (guard)
import           Data.Either                   (isRight)
import           Data.List                     ((\\), elemIndex)
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Text.ParserCombinators.Parsec (eof, Parser, State (stateInput),
                                                char, getParserState, letter,
                                                many1, parse, space, string,
                                                try)

data Rule = Char Char
  | And Rule Rule
  | Or Rule Rule
  | Lookup Int
  deriving(Show, Eq)

pRule :: Parser (Int, Rule)
pRule = pInt .>> string ": "  .>>. (pChar <|> try pOr <|> try pAnd <|> pLookup)
  where
    pChar = Char <$> char '\"' >>. letter
    pLookup = Lookup <$> pInt
    pAnd = uncurry And <$> pLookup .>> space .>>. pLookup
    pAndOrLookup = try pAnd <|> pLookup
    pOr = uncurry Or <$> pAndOrLookup .>> string " | " .>>. pAndOrLookup

readMap :: [String] -> Map Int Rule
readMap = Map.fromList . fmap (parseWith pRule)

createParser :: Map Int Rule -> Rule -> Parser ()
createParser m (Char c)   = () <$ char c
createParser m (Lookup x) = createParser m (m Map.! x)
createParser m (And x y)  = createParser m x >> createParser m y
createParser m (Or x y)   = try (createParser m x) <|> createParser m y

satisfies :: Parser () -> String -> Bool
satisfies p = isRight . parse (p .>> eof) []

partOne :: Map Int Rule -> [String] -> Int
partOne rules = length . filter (satisfies r0)
  where
    r0 = createParser rules (Lookup 0)

{-|
0th Rule is Or (Lookup 8) (Lookup 11)

Rule 8: Matches 1 or more of rule 42
Rule 11 matches "n times" Rule 42 followed by "n times" Rule 31

To check if Rule 0 is satisfied we need to:

* Find humber of consecutive matches of rule 42 (n)
* Find number of consecutive matches of rule 31 (m)
* Check that N > M.

-}

partTwo :: Map Int Rule -> [String] -> Int
partTwo rules =  length . filter matches8Or11
  where

    p42 = try $ createParser rules (Lookup 42)
    p31 = createParser rules (Lookup 31)

    p8Or11Result = do
      n <- length <$> many1 p42
      m <- length <$> many1 p31

      return $ n > m

    matches8Or11 s = 
      case parse (p8Or11Result .>> eof) [] s of
      Right b -> b
      Left _ -> False

main :: IO ()
main = do
  lines <- readDayLines 19
  let Just splitter = elemIndex  "" lines
      (rules, _:samples) = splitAt splitter lines
      ruleLookup = readMap rules

  putStrLn $ "Part One: " ++ show (partOne ruleLookup samples)
  putStrLn $ "Part Two: " ++ show (partTwo ruleLookup samples)