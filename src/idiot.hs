{-# LANGUAGE StrictData #-}
module Main where

import Data.List (foldl')

data RunningTotal = RunningTotal
  { sum :: Int
  , count :: Int
  }

printAverage :: RunningTotal -> IO ()
printAverage (RunningTotal sum count)
  | count == 0 = error "Need at least one value!"
  | otherwise = print (fromIntegral sum / fromIntegral count :: Double)

-- | A fold would be nicer... we'll see that later
printListAverage :: [Int] -> IO ()
printListAverage = printAverage . foldl' f (RunningTotal 0 0)
  where f (RunningTotal sum count) x = RunningTotal (sum + x) (count + 1)

main :: IO ()
main = printListAverage [1..1000000]
