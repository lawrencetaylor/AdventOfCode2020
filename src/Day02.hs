module Day02 where

import           Advent.Parsing                (pDayLines, pNat)
import           Text.Parsec                   (many1, string, char)
import qualified Text.Parsec                   as P
import           Text.Parsec.Char              (lower)
import           Text.ParserCombinators.Parsec (Parser)

-- Model

data Policy = Policy {
    lowest    :: Int
  , highest    :: Int
  , letter :: Char
  } deriving (Show)

type Password = String

-- Parsing

pPolicy :: Parser Policy
pPolicy = do
  min <- pNat
  max <- char '-' >> pNat
  letter <- char ' ' >> lower
  return $ Policy min max letter

pPassword :: Parser Password
pPassword = many1 P.letter

pPasswordPolicy :: Parser (Policy, Password)
pPasswordPolicy = do
  policy <- pPolicy
  password <- string ": " >> pPassword
  return (policy, password)

-- Solution

partOne :: [(Policy, Password)] -> Int
partOne = length . filter verify
  where 
    verify :: (Policy, Password) -> Bool
    verify (policy, password) = 
      let
        c = letter policy
        max = highest policy
        min = lowest policy

        count = 
          length $
          filter (c ==) password
      in count <= max && count  >= min

partTwo :: [(Policy, Password)] -> Int
partTwo = length . filter verify
  where 
    verify :: (Policy, Password) -> Bool
    verify (policy, password) = 
      let
        c = letter policy
        max = highest policy
        min = lowest policy

        positionMatches = 
          length $
          filter (c ==)
          [password !! (min - 1), password !! (max - 1)]

      in positionMatches == 1

main :: IO ()
main = do
  input <- pDayLines 2 pPasswordPolicy
  putStrLn $ "Part One: " ++ show ( partOne input)
  putStrLn $ "Part Two: " ++ show ( partTwo input)