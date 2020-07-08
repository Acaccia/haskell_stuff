module Kata.TargetGame (targetGame) where

import Data.List (tails)

targetGame :: [Int] -> Int
targetGame xs = undefined

allComb :: [Int] -> [[Int]]
allComb = concatMap foo . tails

foo (x:_:xs) = do
  ys <- tails xs
  return (x : ys)
foo _ = [[]]
