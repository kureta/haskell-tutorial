{-# LANGUAGE UnicodeSyntax #-}

module ScratchSpec where

import           Test.Hspec
import           Test.Hspec.QuickCheck

import           Mastermind            (exactMatches)

main ∷ IO ()
main = hspec spec

spec ∷ Spec
spec =
  describe "Mastermind" $ do
    prop "exactMatches is commutative" $ \x y →
      exactMatches x y `shouldBe` exactMatches y x
    prop "Equal lists fully match" $ \x →
      ourAdd x 0 `shouldBe` x
