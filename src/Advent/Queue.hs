module Advent.Queue(
    Queue(Queue)
  , push
  , pushAll
  , pop
  , isEmpty
  ) where

newtype Queue a =
  Queue [a]
  deriving (Show, Eq)

push :: a -> Queue a -> Queue a
push a (Queue q) = Queue (q ++ [a])

pushAll :: [a] -> Queue a -> Queue a
pushAll la q = foldr push q (reverse la)

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue q) =
  case q of
    []     -> Nothing
    (x:xs) -> Just (x, Queue xs)

isEmpty :: Queue a -> Bool
isEmpty (Queue []) = True
isEmpty (Queue _) = False

instance (Ord a) => Ord (Queue a )where
  (Queue a) <= (Queue b) = a <= b

instance Foldable Queue where
  foldMap f (Queue q) = foldMap f (reverse q)