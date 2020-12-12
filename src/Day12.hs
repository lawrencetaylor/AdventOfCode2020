module Day12 where

import           Advent.Parsing                (pDayLines, pInt, (>>.))
import           Advent.Vector                 (Vector (Vector), manhattan)
import           Data.Semigroup                (Sum, getSum)
import           Text.Parsec                   (char, choice)
import           Text.ParserCombinators.Parsec (Parser)

type Direction = Vector (Sum Int) (Sum Int)
type Position = Vector (Sum Int) (Sum Int)

data Action =
  N Int
  | S Int
  | E Int
  | W Int
  | L Int
  | R Int
  | F Int
  deriving (Show)

data Ship = Ship

  { facing   :: Direction
  , position :: Position
  , waypoint :: Position
  }
  deriving (Show)

-- Parsing

pAction :: Parser Action
pAction = choice
    [ N <$> (char 'N' >>. pInt)
      , S <$> (char 'S' >>. pInt)
      , E <$> (char 'E' >>. pInt)
      , W <$> (char 'W' >>. pInt)
      , L <$> (char 'L' >>. pInt)
      , R <$> (char 'R' >>. pInt)
      , F <$> (char 'F' >>. pInt) ]

origin :: Position
origin = Vector 0 0

north :: Direction
north = Vector 0 1

east :: Direction
east = Vector 1 0

south :: Direction
south = Vector 0 (-1)

west :: Direction
west = Vector (-1) 0

rotate90 :: Direction ->  Direction
rotate90 (Vector a b) = Vector (-b) a

rotate270 ::  Direction ->  Direction
rotate270 (Vector a b) = Vector b (-a)

{-| 
This is worth pointing out:

foldr (.) id :: [a -> a] -> (a -> a)
-}
right ::  Int -> Direction ->  Direction
right degrees = foldr (.) id $ replicate turns rotate270
  where
    turns = degrees `div` 90

left :: Int ->  Direction ->  Direction
left degrees = foldr (.) id $ replicate turns rotate90
  where
    turns = degrees `div` 90

move :: Int -> Direction -> Position -> Position
move i d = foldr (.) id $ replicate i (<> d)

moveTo :: Position -> Ship -> Ship
moveTo p s = s { position = p }

turnTo :: (Direction ->  Direction) -> Ship -> Ship
turnTo t s = s { facing = t (facing s)}

moveForward :: Int -> Ship -> Ship
moveForward x s@Ship { facing = f, position = p} =
  let newPosition = move x f p
  in s { facing = f, position = newPosition}

moveWaypointTo :: Position -> Ship -> Ship
moveWaypointTo p s = s { waypoint = p }

turnWaypointTo :: (Direction ->  Direction) -> Ship -> Ship
turnWaypointTo t s = s { waypoint = t (waypoint s)}

moveForwardToWaypoint :: Int -> Ship -> Ship
moveForwardToWaypoint x s@Ship { facing = f, position = p, waypoint = w} =
  let
    newPosition = move x w p
  in s { facing = f, position = newPosition}

getDistanceTravelled :: (Action -> Ship -> Ship) -> [Action] -> Int
getDistanceTravelled f actions = 
  getSum $
  manhattan $
  position $
  foldl (flip f) ship actions
  where
    ship = Ship east origin (Vector 10 1)

partOne :: [Action] -> Int
partOne = getDistanceTravelled act
  where 
    act :: Action -> Ship -> Ship
    act (N x) s = moveTo (move x north $ position s) s
    act (E x) s = moveTo (move x east  $ position s) s
    act (S x) s = moveTo (move x south $ position s) s
    act (W x) s = moveTo (move x west  $ position s) s
    act (L x) s = turnTo (left x) s
    act (R x) s = turnTo (right x) s
    act (F x) s = moveForward x s

partTwo :: [Action] -> Int
partTwo = getDistanceTravelled act
  where 
    act :: Action -> Ship -> Ship
    act (N x) s = moveWaypointTo (move x north $ waypoint s) s
    act (E x) s = moveWaypointTo (move x east  $ waypoint s) s
    act (S x) s = moveWaypointTo (move x south $ waypoint s) s
    act (W x) s = moveWaypointTo (move x west  $ waypoint s) s
    act (L x) s = turnWaypointTo (left x) s
    act (R x) s = turnWaypointTo (right x) s
    act (F x) s = moveForwardToWaypoint x s

main :: IO ()
main = do
  actions <- pDayLines 12 pAction
  putStrLn $ "Part One: "  ++ show (partOne actions)
  putStrLn $ "Part Two: "  ++ show (partTwo actions)