module Advent.Modulo where

{- Represents an element of the ring of Integers modulo n.  -}
data Mod = Mod {
    n     :: Int
  , value :: Int}

instance Eq Mod where
  (Mod n1 v1) == (Mod n2 v2) = 
    n1 == n2 && ((v1 `mod` n1) == (v2 `mod` n1))

{- This instance of Ord doesn't really make arithmetic sense, in the sense that
if x <= y (mod n) it doesn't follow that x + a <= y + a (mod n).  This 
implementation exists to allow us to use Mod as keys in Maps. -}
instance Ord Mod where
  (Mod n1 v1) <= (Mod n2 v2) = (n1 <= n2) && ((v1 `mod` n1) <= (v2 `mod` n1))

instance Show Mod where
  show (Mod n value) = show value ++ "/" ++ show n ++ "Z"

instance Semigroup Mod where
  m1@(Mod n1 v1) <> m2@(Mod n2 v2)
    | n1 == n2 = Mod n1 ((v1 + v2) `mod` n1)
    | otherwise = error $ "Tried to add two Modulo numbers in different rings: " ++ show m1 ++ ", " ++ show m2