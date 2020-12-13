module Advent.Modulo where

{-| Represents an element of the ring of Integers modulo n.  
* n : 
* value -}
data Mod a = Mod {
    n     :: a
  , value :: a}

createMod n value = Mod n (value `mod` n)

instance (Integral a) => Eq (Mod a)  where
  (Mod n1 v1) == (Mod n2 v2) = 
    n1 == n2 && ((v1 `mod` n1) == (v2 `mod` n1))

{- This instance of Ord doesn't really make arithmetic sense, in the sense that
if x <= y (mod n) it doesn't follow that x + a <= y + a (mod n).  This 
implementation exists to allow us to use Mod as keys in Maps. -}
instance (Integral a) =>  Ord (Mod a) where
  (Mod n1 v1) <= (Mod n2 v2) = (n1 <= n2) && ((v1 `mod` n1) <= (v2 `mod` n1))

instance (Show a) => Show (Mod a) where
  show (Mod n value) = show value ++ "/" ++ show n ++ "Z"

instance (Integral a, Show a) =>  Semigroup (Mod a) where
  m1@(Mod n1 v1) <> m2@(Mod n2 v2)
    | n1 == n2 = Mod n1 ((v1 + v2) `mod` n1)
    | otherwise = error $ "Tried to add two Modulo numbers in different rings: " ++ show m1 ++ ", " ++ show m2

{-| 
Calculates the modular inveser of v mod p where P IS A PRIME.

We know that a^(p-1) = 1 (mod p) from standard group theory, 
so a * a^(p-2) (mod p) == 1.
Hence a^(-1) = a^(p-2) (mod p) 
-}
inverse :: (Integral a) => Mod a -> Mod a 
inverse (Mod p v) = Mod p ((v^(p-2)) `mod` p)

createP n p = 
  (* n') $
  value $
  inverse $
  createMod p n'
  where 
    n' = n `div` p

{-|
Solves the Chinese Remainder Theorem.  The input is contrains of the form:

[ (Mod p1 x1), (Mod p2 x2), (Mod p3 x3), ...]

This represents the problem of finding a solution to the congruence:

x = x1 (mod p1)
x = x2 (mod p2)
x = x3 (mod p3)
-}
solveChineseRemainder :: (Integral a) =>  [Mod a] -> a
solveChineseRemainder constraints = sum x `mod` bigN
  where
    primes = n <$> constraints
    coeffs = value <$> constraints
    bigN = product primes
    inverses = createP bigN <$> primes
    x = uncurry (*) <$> zip inverses  coeffs