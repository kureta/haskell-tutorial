{-# LANGUAGE UnicodeSyntax #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module MastermindSpec where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

import           Mastermind            (Color, colorMatches, exactMatches)

instance Arbitrary Color where
  arbitrary = arbitraryBoundedEnum

main ∷ IO ()
main = hspec spec

spec ∷ Spec
spec =
  describe "Mastermind" $ do
    describe "exactMatches" $ do
      prop "is commutative" $ \x y →
        exactMatches x y `shouldBe` exactMatches y x
      it "is zero if lists are empty" $
        exactMatches [] [] `shouldBe` 0
    describe "colorMatches" $
      it "is zero if lists are empty" $
        colorMatches [] [] `shouldBe` 0
