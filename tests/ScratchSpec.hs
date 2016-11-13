{-# LANGUAGE UnicodeSyntax #-}

module ScratchSpec where

import           Test.Hspec
import           Test.Hspec.QuickCheck

import           Scratch                   (ourAdd)

main ∷ IO ()
main = hspec spec

spec ∷ Spec
spec =
  describe "Scratch" $
    describe "ourAdd" $ do
      prop "is commutative" $ \x y →
        ourAdd x y `shouldBe` ourAdd y x
      prop "has identity 0" $ \x →
        ourAdd x 0 `shouldBe` x
