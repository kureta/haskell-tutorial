{-# LANGUAGE UnicodeSyntax #-}

module Mastermind () where

import           Control.Applicative (liftA2)
import           Control.Arrow       (Arrow, (&&&))
import           Data.Function       (on)
import           Prelude.Unicode

merge ∷ (Arrow a, Applicative f) ⇒ f (a b c) → f (a b c') → f (a b (c, c'))
merge = liftA2 (&&&)

comp12 ∷ (Functor f, Functor f1) ⇒ (a → b) → f (f1 a) → f (f1 b)
comp12 = fmap ∘ fmap

comp22 ∷ (Applicative f, Functor f1) ⇒ f (a → b) → f (f1 a) → f (f1 b)
comp22 = liftA2 fmap

data Color = Red | Green | Blue | Yellow | Black | White
           deriving (Show, Eq, Ord, Bounded, Enum)

colors ∷ [Color]
colors = [minBound ∷ Color ..]

code ∷ [Color]
code = [Red, Green, Red, Blue]

guess ∷ [Color]
guess = [Red, Blue, Green, Blue]

exactMatches ∷ [Color] → [Color] → Int
exactMatches = (length ∘ filter id) `comp12` zipWith (≡)

colorCounters ∷ [[Color] → Int]
colorCounters = map (length `comp12` (filter ∘ (≡))) colors

countColors ∷ [Color] → [Int]
countColors = sequence colorCounters

colorMatches ∷ [Color] → [Color] → Int
colorMatches = sum `comp12` zipWith min `on` countColors

data Matches = Matches{ exact ∷ Int
                      , color ∷ Int }
               deriving (Show)

mastermind ∷ [Color] → [Color] → Matches
mastermind = uncurry (Matches `comp22` subtract) `comp12` merge exactMatches colorMatches
