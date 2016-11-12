{-# LANGUAGE UnicodeSyntax #-}

module ScratchSpec where

import           Test.Hspec
import           Test.Hspec.QuickCheck

import           Scratch                   (ourAdd)

main ∷ IO ()
main = hspec spec

spec ∷ Spec
spec =
  describe "Scratch" $ do
    prop "ourAdd is commutative" $ \x y →
      ourAdd x y `shouldBe` ourAdd y x
    prop "Zero is identity" $ \x →
      ourAdd x 0 `shouldBe` x
