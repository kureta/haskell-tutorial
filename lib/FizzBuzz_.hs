module Scratch where

import qualified Control.Foldl as F
import qualified Data.Composition as C
import Data.String

basePredicate :: Int -> Int -> Bool
basePredicate = (== 0) `C.compose2` flip mod

wordFilter :: String -> (Int -> Bool) -> Int -> String
wordFilter = flip F.filtered . const

word :: String -> Int -> Int -> String
word = (. basePredicate) . wordFilter

multiPredicate :: [Int] -> [Int -> Bool]
multiPredicate = map basePredicate

multiTruth :: [Int] -> Int -> [Bool]
multiTruth = sequence . multiPredicate

combinedTruth :: [Int] -> Int -> Bool
combinedTruth = (all not .) . multiTruth

numFilter :: (Int -> Bool) -> Int -> String
numFilter = flip F.filtered show

number :: [Int] -> Int -> String
number = numFilter . combinedTruth

elemWiseConcat :: [String] -> [String] -> [String]
elemWiseConcat = ($) zipWith (++)

result :: [String]
result = foldl1 elemWiseConcat [nums, fizz, buzz]
  where
    nums = map (number [3, 5]) [1 ..]
    fizz = map (word "fizz" 3) [1 ..]
    buzz = map (word "buzz" 5) [1 ..]
