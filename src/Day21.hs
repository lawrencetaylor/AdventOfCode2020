{-# LANGUAGE TupleSections #-}
module Day21 where

import           Advent.Parsing                           (pDayLines,
                                                           readDayLines, (.>>),
                                                           (.>>.), (>>.))
import qualified Data.Bifunctor
import           Data.List                                (intercalate, sortOn)
import           Data.Map.Strict                          (Map)
import qualified Data.Map.Strict                          as Map
import           Data.Set                                 (Set)
import qualified Data.Set                                 as Set
import           Text.ParserCombinators.Parsec            (Parser, sepBy,
                                                           string)
import           Text.ParserCombinators.Parsec.Char       (letter, space)
import           Text.ParserCombinators.Parsec.Combinator (many1)

type Food = String
type Allergen = String
type Label = ([Food], [Allergen])

-- Parsing

pWord :: Parser String
pWord = many1 letter

pFood :: Parser Food
pFood = pWord .>> space

pIngredients :: Parser [Food]
pIngredients = many1 pFood

pAllergens :: Parser [Allergen]
pAllergens =
   string "(contains " >>. sepBy pWord (string ", ")

pLabel :: Parser Label
pLabel = pIngredients .>>. pAllergens


toCandidate :: Label -> [(Allergen, Set Food)]
toCandidate (foods, allergens) = (, Set.fromList foods) <$> allergens

{-|
Adds the current (Allgen, Foods) to the current collection of mappings
Allergen -> Food that MAY contain that allergen
-}
addAllergen ::(Allergen, Set Food) -> Map Allergen [Set Food] -> Map Allergen [Set Food]
addAllergen (allergen, foods) = Map.insertWith (++) allergen [foods]

{-|
From the Label of ingrediencts create a map
Allergen -> Food that MAY contain that allergen
-}
unsafeCandidates :: [Label] -> Map Allergen [Set Food]
unsafeCandidates labels = foldr addAllergen Map.empty l
  where
    l = concatMap toCandidate labels

{-
From a mapping of
Allergen -> Food that MAY contain that allergen
returns the set of all foods that cannot be assumed to be safe.
-}
unsafeFoods :: Map Allergen (Set Food) -> Set Food
unsafeFoods = foldl1 Set.union . fmap snd . Map.toList

-- partOne :: [Label] ->  (Int, Set Food)
partOne :: [Label] -> (Int, Map Allergen [Set Food], Set Food)
partOne label = (length x, m, safeFoods)
  where
    m = unsafeCandidates label
    trimmed = Map.map (foldl1 Set.intersection) m
    ingredientsWithAllergens = unsafeFoods trimmed
    allIncredicnets = unsafeFoods $ Map.map (foldl1 Set.union) m
    safeFoods = allIncredicnets `Set.difference` ingredientsWithAllergens
    allFoods = Map.toList m
    x =  [ isInFoods | f <- Set.toList safeFoods, (foods, _) <- label, let isInFoods = f `elem` foods, isInFoods ]

{-|
Same algorithm as Dy 16.
Essentially does guassian elimination on a triangular matrix
-}
assignFoods :: [(Allergen, [Food])] -> Map Allergen Food
assignFoods = inner Map.empty
  where
    inner :: Map Allergen Food -> [(Allergen, [Food])] -> Map Allergen Food
    inner m [] = m
    inner m outstanding = inner newM xs
      where
        assigned = Map.elems m
        removeAssigned = filter (`notElem` assigned)
        ((i, [h]): xs) =
          sortOn (length . snd) $
          Data.Bifunctor.second removeAssigned <$> outstanding
        newM = Map.insert i h m

partTwo :: Map Allergen [Set Food] -> Set Food -> String
partTwo m safeFoods = m'
  where

    removeSafe  = filter (not . (`Set.member` safeFoods))

    m' =
      intercalate "," $
      fmap snd $
      sortOn fst $                                          -- Sort on allergen name
      Map.toList $
      assignFoods $                                        -- Run simplified "guassian elimination"
      Map.toList $
      Map.map ( Set.toList . foldr1 Set.intersection) $     -- Only include foods that are in ALL the ingredient lists for each allergen
      Map.map (\x -> (`Set.difference` safeFoods) <$> x) m  -- Remove known SAFE foods

main :: IO ()
main = do
  labels <- pDayLines 21 pLabel
  let (partOneAnswer, allergenFoodMap,  safeFoods) = partOne labels
  putStrLn $ "Part One: " ++ show partOneAnswer
  putStrLn $ "Part Two: " ++ show ( partTwo allergenFoodMap safeFoods)
