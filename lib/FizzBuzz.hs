{-# LANGUAGE UnicodeSyntax #-}

module FizzBuzz () where

import           Control.Arrow
import           Prelude.Unicode

(+^+) ∷ [String] → [String] → [String]
(+^+) = zipWith (++)

makeRule ∷ (Int, String) → [String]
makeRule = cycle ^<< uncurry (++) ^<< (makeHoles *** (: []))
           where makeHoles = flip replicate "" ^<< subtract 1

combineRules ∷ [(Int, String)] → [String]
combineRules = foldr1 (+^+) ^<< map makeRule

myRules ∷ [String]
myRules = combineRules [(3, "fizz"), (5, "buzz"), (7, "quizno")]

fizzbuzz ∷ [String]
fizzbuzz = zipWith insertNums myRules [1..]
           where insertNums word number = if null word
                                         then show number
                                         else word
