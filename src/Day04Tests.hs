module Day04Tests where

import           Day04

import           Advent.Parsing (pInt)
import           Control.Monad  (forM_)
import           Test.Hspec     (describe, hspec, it, shouldBe)

testDay04 :: IO ()
testDay04 = hspec $ do
  describe "Between tests" $ do
    it "Before Lower Boundary" $ between 100 200 99 `shouldBe` False
    it "On Lower Boundary" $ between 100 200 100 `shouldBe` True
    it "Between Boundaries" $ between 100 200 101 `shouldBe` True
    it "On Upper Boundary" $ between 100 102 102 `shouldBe` True
    it "After Upper Boundary" $ between 100 102 103 `shouldBe` False

  describe "Birth Year Tests" $ do
    let isValidBirthYear = isValid pInt (between 1920 2002)
    it "False for 1900" $  isValidBirthYear "1900" `shouldBe` False
    it "True for 1921" $  isValidBirthYear "1921" `shouldBe` True
    it "Too many digits" $  isValidBirthYear "19000" `shouldBe` False
    it "Not a number" $  isValidBirthYear "1900d" `shouldBe` False
    it "Trailing space" $  isValidBirthYear "1900 " `shouldBe` False

  describe "Height Tests" $ do
    let isValidHeight = isValid pHeight (uncurry checkHeight)
    it "Below Lower Bound (149cm)" $  isValidHeight "149cm" `shouldBe` False
    it "Lower Bound (150cm)" $  isValidHeight "150cm" `shouldBe` True
    it "Within Bound (151cm)" $  isValidHeight "151cm" `shouldBe` True
    it "Upper Bound (193cm)" $  isValidHeight "193cm" `shouldBe` True
    it "Above Upper Bound (194cm)" $  isValidHeight "194cm" `shouldBe` False
    it "Below Lower Bound (58in)" $  isValidHeight "58in" `shouldBe` False
    it "Lower Bound (59in)" $  isValidHeight "59in" `shouldBe` True
    it "Within Bound (60in)" $  isValidHeight "60in" `shouldBe` True
    it "Upper Bound (76in)" $  isValidHeight "76in" `shouldBe` True
    it "Above Upper Bound (77in)" $  isValidHeight "77in" `shouldBe` False
    it "Above Upper Bound (190in)" $  isValidHeight "190in" `shouldBe` False
    it "Wrong units (inch)" $  isValidHeight "77inch" `shouldBe` False
    it "No units" $  isValidHeight "190" `shouldBe` False

  describe "Hair Colour Tests" $ do
    let isValidHairColour = isValid pHairColor (const True)
    it "Correct Value" $  isValidHairColour "#123abc" `shouldBe` True
    it "Invalid letters" $  isValidHairColour "#123abg" `shouldBe` False
    it "Too few characters" $  isValidHairColour "#12312" `shouldBe` False
    it "Too many characters" $  isValidHairColour "#1231234" `shouldBe` False
    it "No leading #" $  isValidHairColour "123123" `shouldBe` False

  describe "Eye Colour Tests" $ do
    let isValidEyeColour = isValid pEyeColor (const True)
    let validColours = ["amb", "blu", "brn", "gry", "grn", "hzl" ,"oth" ]
    it "Invalid colour" $  isValidEyeColour "abc" `shouldBe` False
    forM_ validColours (\e ->
      it ("Correct Value (" ++ e ++")") $  isValidEyeColour e `shouldBe` True)

  describe "Password Tests" $ do
    let isValidPassword = isValid pPassportId (const True)
    it "Correct Value (All zeros)" $  isValidPassword "000000000" `shouldBe` True
    it "Correct Value (No zeros)" $  isValidPassword "123456789" `shouldBe` True
    it "Too few Characters" $  isValidPassword "00000000" `shouldBe` False
    it "Too many Characters" $  isValidPassword "0000000000" `shouldBe` False
    it "Contains letters" $  isValidPassword "0000a0000" `shouldBe` False
