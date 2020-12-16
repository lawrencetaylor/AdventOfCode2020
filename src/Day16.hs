module Day16 where

import           Advent.Parsing                (pDay, pInt, (.>>))
import qualified Data.Bifunctor
import           Data.List                     (isPrefixOf, sortOn)
import qualified Data.Map                      as M
import           Text.ParserCombinators.Parsec (Parser, char, letter, many,
                                                newline, sepBy, string, (<|>))

type Ticket = [Int]
type Rule = Int -> Bool
type Field = (String, Rule)

data Input = Input {
    fields        :: [Field]
  , myTicket      :: Ticket
  , nearbyTickets :: [Ticket]
  }

-- Parsing

pRange :: Parser Rule
pRange = do
  lower <- pInt .>> char '-'
  upper <- pInt
  return (\i -> i >= lower && i <= upper)

pAttribute :: Parser Field
pAttribute = do
  name <- many (letter <|> char ' ') .>> string ": "
  range1 <- pRange .>> string " or "
  range2 <- pRange .>> newline
  return  (name, \i -> range1 i || range2 i)

pTicket :: Parser [Int]
pTicket = sepBy pInt (char ',')

pInput :: Parser Input
pInput = do
  fields <- many pAttribute .>> newline
  _ <- string "your ticket:" >> newline
  myTicket <- pTicket .>> newline .>> newline
  _ <- string "nearby tickets:" >> newline
  nearbyTickets <- sepBy pTicket newline
  return $ Input fields myTicket nearbyTickets

-- Solution

validForAny :: [Field] ->  Int -> Bool
validForAny fields x = any (`snd` x) fields

removeInvalid :: Input -> Input
removeInvalid (Input fields mine nearby) = Input fields mine valid
  where
    allVAlidForAny = all $ validForAny fields
    valid = filter allVAlidForAny nearby

{-|
Gets all the fields that are candidates for the i'th
number on the ticket.  i.e.  Those attributes for which the i-th number
of all the nearby tickets satisfy the field rule.
-}
getValid :: Int -> [Field] -> [Ticket] -> [String]
getValid i attributes tickets = fst <$> (filter $ isValid i tickets) attributes
  where
    isValid i  tickets (_, r) = all r [ t !! i | t <- tickets]

{-|
Create a sequence of pairs (i, field names), where each of the named fields is a
valid candidate for the field stored in the i-th position of the ticket.
-}
candidateAttributes :: Input -> [(Int, [String])]
candidateAttributes (Input fields mine nearby) = x
  where
    ticketLength = length mine
    x = fmap (\i -> (i,getValid i fields nearby)) [0..ticketLength-1]

{-|
Assigns (i -> Field) by iteratively looking at which fields can be uniquely
assigned to a field index and then removing those for the next iteration.
-}
assignFields :: [(Int, [String])]  -> M.Map Int String
assignFields = inner M.empty
  where
    inner :: M.Map Int String -> [(Int, [String])] -> M.Map Int String
    inner m [] = m
    inner m outstanding = inner newM xs
      where
        assigned = M.elems m
        removeAssigned = filter (`notElem` assigned)
        ((i, [h]): xs) =
          sortOn (length . snd) $
          Data.Bifunctor.second removeAssigned <$> outstanding
        newM = M.insert i h m


partOne :: Input -> Int
partOne (Input fields _ nearby) = sum all
  where
    all = filter (not . validForAny fields) $ concat nearby

partTwo :: Input -> Int
partTwo input =
  product $
  fmap snd $
  M.toList $                             -- Convert Map to List
  M.mapWithKey (\k _ -> mine !! k) $     -- Pick out those fields for MY ticket
  M.filter (isPrefixOf  "departure") $   -- Only look at those "departure" fields
  assignFields $                         -- Assign fields to correct index
  candidateAttributes valid              -- Get valid fields for each index
  where
    valid@(Input _ mine _) = removeInvalid input

main :: IO()
main = do
  input <- pDay 16 pInput
  putStrLn $ "Part One: " ++ show ( partOne input)
  putStrLn $ "Part Two: " ++ show ( partTwo input)
