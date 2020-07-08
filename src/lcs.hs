module LongestCommonSubsequence where
import Data.Array
import Data.List
import Data.Ord

lcs :: String -> String -> String
lcs xs ys = arr ! (maxX, maxY) where
  maxX = length xs
  maxY = length ys
  xv = listArray (0, maxX) xs
  yv = listArray (0, maxY) ys
  arr = listArray ((0, 0), (maxX, maxY)) [foo x y | x <- [0..maxX], y <- [0..maxY]]
  foo 0 _ = ""
  foo _ 0 = ""
  foo x y | xv ! (x-1) == yv ! (y-1) = arr ! (x-1, y-1) ++ [xv ! (x-1)]
          | otherwise = maximumBy (comparing length) [arr ! (x-1, y), arr ! (x, y-1)]
