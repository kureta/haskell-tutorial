{-# LANGUAGE UnicodeSyntax #-}

module Euclid where

import           Music.Theory.Bjorklund
import           Prelude.Unicode

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate n xs = zipWith const (drop n (cycle xs)) xs

