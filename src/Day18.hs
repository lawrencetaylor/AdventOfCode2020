{-# LANGUAGE OverloadedStrings #-}

module Day18 where

import           Advent.Parsing                 (readDayLines)
import           Control.Monad.Combinators.Expr (Operator (InfixL),
                                                 makeExprParser)
import           Control.Monad.State            (runState)
import           Data.Text                      (Text, pack)
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer     as L


{- 
I was today years old when I learnt that there is built in functions for parsing
Expressions (recursive ADTs).  

This solution shamelessly follows the technique in https://markkarpov.com/tutorial/megaparsec.html#parsing-expressions.
-}

-- Some Helper Functions

type Parser = Parsec Void Text

{-| White space parser-}
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

-- "Model" for the Expression tree

data Expr = 
  Add Expr Expr 
  | Times Expr Expr 
  | Value Int deriving (Show)

evaluate :: Expr -> Int
evaluate (Value v)     = v
evaluate (Add e1 e2)   = evaluate e1 + evaluate e2
evaluate (Times e1 e2) = evaluate e1 * evaluate e2

-- Parsing the Expression Tree

pInteger :: Parser Expr
pInteger = Value <$> lexeme L.decimal

pExpr :: [[Operator Parser Expr]] -> Parser Expr
pExpr operatorTable = makeExprParser pTerm operatorTable
  where
    pTerm :: Parser Expr
    pTerm = choice
      [ parens (pExpr operatorTable)
      , pInteger
      ]


-- Solution

eval :: Parser Expr -> String -> Int
eval parser str =
  let p =  runParser parser "" $ pack str
      (Right exp) = p
  in evaluate exp

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary  name f = InfixL  (f <$ symbol name)

partOne :: [String] -> Int
partOne lines = sum $ eval(pExpr operatorTable) <$> lines
  where
    operatorTable =
      [ [ binary "*" Times  , binary "+" Add  ] ]

partTwo:: [String] -> Int
partTwo lines = sum $ eval(pExpr operatorTable) <$> lines
  where
      operatorTable =
        [ [ binary "+" Add  ]
        , [ binary "*" Times ]
        ]

main :: IO ()
main = do
  lines <- readDayLines 18
  putStrLn $ "Part One: " ++ show ( partOne lines)
  putStrLn $ "Part Two: " ++ show ( partTwo lines)