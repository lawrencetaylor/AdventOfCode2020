module Advent.Graph(
  bfs
  ) where

import           Advent.Queue (Queue (Queue))
import qualified Advent.Queue as Queue
import qualified Data.List    as List
import qualified Data.Maybe   as Maybe
import qualified Data.Set     as Set


{-|
Conducts a breadth-first search on a graph
-}
bfs :: (Ord b) =>
  (a -> [a])   -- ^ Defines all the NEXT values from a given starting point
  -> (a -> b)  -- ^ Defines the value we associate to all nodes of the graph
  -> a         -- ^ Defines the starting node
  -> [a]       -- ^ Defines all nodes in the graph
bfs
  next
  rep
  a =
  Maybe.catMaybes $
  List.unfoldr (bfsUnfold next) (Set.empty, Queue [a])
  where
    bfsUnfold next (seen, q) =
      case Queue.pop q of
        Nothing -> Nothing
        Just (p, qs) ->
          if rep p `Set.member` seen then
            Just (Nothing, (seen, qs))
          else
              Just (Just p, (Set.insert (rep p) seen, Queue.pushAll (next p) qs))
