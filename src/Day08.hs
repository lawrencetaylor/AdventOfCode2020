module Day08 where

import           Advent.Parsing                (pDayLines, pInt, (>>.))
import           Data.IntSet                   (IntSet, empty, insert, member)
import           Data.List                     (nub, unfoldr)
import           Data.Map                      (Map, fromList, (!?))
import           Text.Parsec                   (choice, string)
import           Text.ParserCombinators.Parsec (Parser)

-- Model

data Instruction = Acc Int | Jmp Int | Nop Int
  deriving (Show, Eq  )

type Program = Map Int Instruction

data Executable = Executable {
    program            :: Program
  , value              :: Int
  , instructionPointer :: Int
  , seen               :: IntSet
  }

-- Parsing

pInstruction :: Parser Instruction
pInstruction = choice [
    Acc <$> string "acc " >>. pInt
  , Nop <$> string "nop " >>. pInt
  , Jmp <$> string "jmp " >>. pInt
  ]

-- Solution

data IterationResult =
  StillRunning Int
  | DetectedRepeat
  | EndOfInstructions Int
  deriving (Show)

incrementPosition :: Int -> Executable -> Executable
incrementPosition i e@Executable{ instructionPointer = oldPosition } = e { instructionPointer = oldPosition + i }

accumulate :: Int -> Executable -> Executable
accumulate i e@Executable{ value = oldValue } = e { value = oldValue + i }

logSeen :: Int -> Executable -> Executable
logSeen i e@Executable{ seen = oldSeen } = e { seen = insert i oldSeen}

{-|
Executes the instruction referenced by the current instruction pointer.
This function is used within an "unfold" to create a sequence of "IterationResult".
-}
executeCurrent :: Executable -> Maybe (IterationResult, Executable)
executeCurrent e@Executable{
        program = program
      , instructionPointer = i
      , seen = seen
      , value = value} =

    case program !? i of
      Just instruction ->
        let e' = case instruction of
                  Acc a -> ( logSeen i . incrementPosition 1 . accumulate a ) e
                  Jmp j -> ( logSeen i . incrementPosition j ) e
                  Nop _ -> ( logSeen i . incrementPosition 1 ) e
        in  if i `member` seen
              then Just (DetectedRepeat, e')
            else Just (StillRunning value, e')

      Nothing -> Just (EndOfInstructions value,e )

{-|
Creates all possible programs from an initial one by
* Changing one Nop instruction to Jmp
* Changing one Jmp instruction to Nop
-}
possiblePrograms :: [Instruction] -> [[Instruction]]
possiblePrograms instructions =
  nub [ a ++ [modify x] ++ xs |
            i <- [0..l-1]
          , let
              (a, b) = splitAt i instructions
              (x : xs) = b]
  where
    l = length instructions

    modify :: Instruction -> Instruction
    modify (Nop i) = Jmp i
    modify (Jmp i) = Nop i
    modify x       = x

{-|
Runs the specified program of instructions to completion.  In the first
argument of the returned tuple:

* "True" indicates the program finished due to running out of instructions;
* "False" indicaes the program was terminated due to an infinite loop.
-}
run :: [Instruction] -> (Int, Bool)
run instructions = findLastAcc iterations
  where
    p = toProgram instructions
    e = loadProgram p
    iterations = unfoldr executeCurrent e

    toProgram :: [Instruction] -> Program
    toProgram instructions = fromList $ zip [0..] instructions

    findLastAcc :: [IterationResult] -> (Int, Bool)
    findLastAcc (StillRunning v : DetectedRepeat: _) = (v, False)
    findLastAcc (EndOfInstructions v: _)             = (v, True)
    findLastAcc (_:xs)                               = findLastAcc xs

    loadProgram :: Program -> Executable
    loadProgram prog = Executable prog 0 0 empty

partOne :: [Instruction] -> Int
partOne = fst . run

partTwo :: [Instruction] -> Int
partTwo instructions =
  fst $
  head $
  filter snd $ run <$> possiblePrograms instructions

main :: IO ()
main = do
  input <- pDayLines 8 pInstruction
  putStrLn $ "Part One: " ++ show ( partOne input)
  putStrLn $ "Part Two: " ++ show ( partTwo input)
