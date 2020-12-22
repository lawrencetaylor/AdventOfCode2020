module Day22 where

import           Advent.Parsing                (pInt, parseWith, readDay,
                                                readDayLines, (.>>), (>>.))
import           Advent.Queue                  (Queue (Queue))
import qualified Advent.Queue                  as Queue
import           Data.List                     (unfoldr)
import           Data.List.Split               (splitOn)
import           Data.Set                      (Set)
import qualified Data.Set                      as Set
import           Text.ParserCombinators.Parsec (Parser, char, newline, sepBy,
                                                string)

type Deck = Queue Int

-- Parsing

pDeck :: Parser Deck
pDeck = do
  player <- string "Player " >>. pInt .>> char ':' .>> newline
  cards <- sepBy pInt newline
  return $ Queue cards

-- Solution

draw :: [Deck] -> Maybe ([Deck], [Deck])
draw [q1, q2] = do
  (c1, q1') <- Queue.pop q1
  (c2, q2') <- Queue.pop q2
  let y = if c1 > c2 then [ Queue.pushAll [c1, c2] q1', q2']  else [ q1', Queue.pushAll [c2, c1] q2']
  return (y, y)

playRecursive :: (Deck, Deck, Set [Deck]) -> Either Int Int
playRecursive (q1, q2, previous) =
  if [q1, q2] `Set.member` previous then Left $ score q1
  else do
      let previous' = Set.insert [q1, q2] previous
      case (Queue.pop q1, Queue.pop q2) of
        (Nothing, Just (c2, q2'@(Queue qs2))) -> Right $ score q2
        (Just (c1, q1'@(Queue qs1)), Nothing) -> Left $ score q1
        (Just (c1, q1'@(Queue qs1)),Just (c2, q2'@(Queue qs2)) ) ->
          if c1 > length qs1 || c2 > length qs2
            then
              if c1 > c2 then
                playRecursive ( Queue.pushAll [c1, c2] q1', q2', previous')
              else playRecursive ( q1', Queue.pushAll [c2, c1] q2', previous')
          else
            case playRecursive (Queue $ take c1 qs1, Queue $ take c2 qs2, Set.empty) of
            Left _ -> playRecursive ( Queue.pushAll [c1, c2] q1', q2', previous')
            Right _ -> playRecursive ( q1', Queue.pushAll [c2, c1] q2', previous')

score :: Deck -> Int
score (Queue winningCards) = sum $ zipWith (*) (reverse winningCards) [1..]

winnersScore :: [[Queue Int]] -> Int
winnersScore game = score $ head $ filter( not . Queue.isEmpty) $ last game

partOne :: Deck -> Deck -> Int
partOne deck1 deck2 =
  score $
  head $
  filter( not . Queue.isEmpty) $      -- Find one that is non-empty
  last $                              -- Get final state of Decks
  unfoldr draw [deck1, deck2]         -- Play the game

partTwo :: Deck -> Deck -> Int
partTwo deck1 deck2 =
  case playRecursive (deck1, deck2, Set.empty) of
    Left score  -> score
    Right score -> score

main :: IO ()
main = do
  text <- readDay 22
  let decks = splitOn "\n\n" text
  let [deck1, deck2] = parseWith pDeck <$> decks
  putStrLn $ "Part One: " ++ show (partOne deck1 deck2)
  putStrLn $ "Part Two: " ++ show ( partTwo deck1 deck2)
