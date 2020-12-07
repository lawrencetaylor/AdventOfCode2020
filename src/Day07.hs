module Day07 where

import           Advent.Graph
import           Advent.Parsing                (pDayLines, pInt, (.>>))
import           Data.List                     (delete)
import           Data.Map                      (Map, fromList, keys, (!))
import           Text.ParserCombinators.Parsec (Parser, char, letter, many,
                                                sepBy, string, try, (<|>))


-- Model

type Bag = String

data Rule = Rule {
    bag      :: Bag
  , contents :: [(Int, Bag)]
  } deriving (Eq, Show)

type Graph = Map Bag [(Int, Bag)]

-- Parsing

pBag :: Parser Bag
pBag = do
  description <- many letter .>> char ' '
  colour <- many letter
  _ <- try (string " bags") <|> try (string " bag")
  return $ description ++ " " ++ colour

pChildBag :: Parser (Int, Bag)
pChildBag = do
  count <- pInt .>> char ' '
  bag <- pBag
  return (count, bag)

pRule :: Parser Rule
pRule = do
  bag <- pBag .>> string " contain "
  children <- sepBy pChildBag (string ", ")
  return $ Rule bag children

toGraph :: [Rule] -> Graph
toGraph rules = fromList $ (\t -> (bag t, contents t)) <$> rules

--  Solutions

bagCount :: Graph -> Bag -> Int
bagCount g b
  | null (g ! b) = 1
  | otherwise =
    let
      thisBagCount = 1
      childCount = sum $ fmap (\(i, b') -> i * bagCount g b') ( g ! b)
    in thisBagCount + childCount

partOne :: Graph -> Int
partOne g =
  length $ filter (elem "shiny gold") $ reachableBags <$>  otherKeys
  where
    otherKeys = delete "shiny gold" $ keys g

    -- BFS to find all "reachable" bags from a given bag

    -- Gets "child bags"
    next :: Bag -> [Bag]
    next = fmap snd . (!) g

    reachableBags :: Bag -> [Bag]
    reachableBags = bfs next id

partTwo :: Graph ->  Int
partTwo g =
  -- "- 1" to not count the outermost "shiny gold" bag.
  bagCount g "shiny gold" - 1

main :: IO ()
main = do
  input <- toGraph <$> pDayLines 7 pRule
  putStrLn $ "Part One: " ++ show ( partOne input)
  putStrLn $ "Part Two: " ++ show ( partTwo input)