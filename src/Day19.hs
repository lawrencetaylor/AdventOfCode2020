module Day19 where


import Text.ParserCombinators.Parsec (many1, eof,  try, string, space, sepBy, letter, char, Parser)
import Advent.Parsing(readDayLines, (.>>.), (.>>), pInt, (>>.), parseWith)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Control.Applicative (Alternative((<|>)))
import Data.List (intercalate, elemIndex)

test = 
  [ "0: 1 2"
  , "1: \"a\""
  , "2: 1 3 | 3 1"
  , "3: \"b\"" ]

type Sequence = [Int]

data Rule = 
  Character Char 
  | Sequence [Int] deriving (Show) 

type Rules = Map Int [Rule]

pCharacter :: Parser [Rule]
pCharacter = fmap (: []) Character <$> (char '\"' >>. letter)

pSequence :: Parser Rule
pSequence = Sequence <$> many1 (pInt .>> (() <$ space <|> eof))

pSequences :: Parser [Rule]
pSequences = sepBy (try pSequence) (try $ string "| ")

pRule :: Parser (Int, [Rule])
pRule = pInt .>> string ": "  .>>. (pCharacter <|> pSequences)

pRules :: [String] -> Rules
pRules = Map.fromList . fmap (parseWith pRule)

isValid :: Rules -> [Rule] -> String ->  Maybe String
isValid rules [Character c] (x:xs) = if x == c then  Just xs else Nothing
isValid rules [] x  = Just x
isValid rules [Sequence []] x  = Just x
isValid rules [Sequence (r:rs)] x  = isValid rules (rules Map.! r) x >>= isValid rules [Sequence rs]
isValid rules ruleSet x  = foldl1 (<|>) $ fmap (\r -> isValid rules [r] x) ruleSet

partOne :: Rules -> [String] -> Int
partOne rules  = length . filter check
  where
    check str = isValid rules (rules Map.! 0) str == Just ""

main :: IO ()
main = do
  lines <- readDayLines 19
  let Just splitter = elemIndex  "" lines
  let (rules, _:samples) = splitAt splitter lines
  let ruleLookup = pRules rules
  putStrLn $ "Part One: " ++ show ( partOne ruleLookup samples)
