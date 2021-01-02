module Advent.Monad(
  iterateM
  , applyM
  ) where


iterateM :: (Monad m) => Int -> (a -> m a) -> a -> m [a]
iterateM 0 _ _ = return []
iterateM n f a = f a >>= (\y ->  do ys <- iterateM (n-1) f y; return $ y:ys)

applyM :: Int -> (a -> IO a ) -> a -> IO a
applyM 0 f a = return a
applyM n f a = do
    y <- f a
    applyM (n - 1) f y
