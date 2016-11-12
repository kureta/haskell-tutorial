{-# LANGUAGE UnicodeSyntax #-}

module FizzBuzz () where

import           Prelude.Unicode

(∘∘) ∷ (Functor f, Functor f1) ⇒ (a → b) → f (f1 a) → f (f1 b)
(∘∘) = fmap ∘ fmap

(+++) ∷ [String] → [String] → [String]
(+++) = zipWith (++)

makeRule ∷ Int → String → [String]
makeRule num word = cycle (replicate (num - 1) "" ++ [word])

combineRules ∷ [Int] → [String] → [String]
combineRules = foldr1 (+++) ∘∘ zipWith makeRule

rules ∷ [String]
rules = combineRules [3, 5, 7] ["fizz", "buzz", "quizno"]

machine ∷ [String]
machine = zipWith merge rules [1..]
          where merge word number = if null word
                                      then show number
                                      else word
