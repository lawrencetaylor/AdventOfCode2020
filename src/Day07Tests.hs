module Day07Tests where

import           Day07

import           Advent.Parsing (parseWith)
import           Test.Hspec     (describe, hspec, it, shouldBe)

test :: IO ()
test = hspec $ do
  describe "Bag Parsing Tests" $ do
    let str = "striped beige bags contain 5 dull beige bags."
    it "Can parse bag" $ parseWith pBag str `shouldBe` "striped beige"
  describe "Child Bag Parsing Tests" $ do
    it "Can parse multiple child bags" $ parseWith pChildBag "5 dull beige bags" `shouldBe` (5, "dull beige")
  describe "Rule Parsing Tests" $ do
    it "No child bags" $ parseWith pRule "faded blue bags contain no other bags." `shouldBe` Rule "faded blue" []
    it "One child bag" $ parseWith pRule "striped beige bags contain 5 dull beige bags." `shouldBe` Rule "striped beige" [(5, "dull beige")]
    it "Two child bags" $ parseWith pRule "light red bags contain 1 bright white bag, 2 muted yellow bags." `shouldBe` Rule "light red" [(1,"bright white"),(2,"muted yellow")]
    it "Four child bags" $ parseWith pRule "posh olive bags contain 4 drab maroon bags, 2 vibrant crimson bags, 3 vibrant aqua bags, 1 dotted cyan bag."  `shouldBe` Rule "posh olive" [(4,"drab maroon"),(2,"vibrant crimson"), (3, "vibrant aqua"), (1, "dotted cyan")]
