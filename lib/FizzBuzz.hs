{-# LANGUAGE UnicodeSyntax #-}

module FizzBuzz () where

import           Prelude.Unicode

(+++) ∷ [String] → [String] → [String]
(+++) = zipWith (++)

fizzes ∷ [String]
fizzes = cycle ["", "", "fizz"]

buzzes ∷ [String]
buzzes = cycle ["", "", "", "", "buzz"]

quiznos ∷ [String]
quiznos = cycle ["", "", "", "", "", "", "quizno"]

rules ∷ [String]
rules = fizzes +++ buzzes +++ quiznos

machine ∷ [String]
machine = zipWith merge rules [1..]
          where merge word number = if null word
                                      then show number
                                      else word
