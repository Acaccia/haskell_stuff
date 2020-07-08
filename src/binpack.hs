module Binpack where

import Data.Array

distributeGifts :: Int -> [(Int,Int)] -> Int
distributeGifts maxW xs = arr ! (len, maxW)
  where (ws, vs) = unzip $ (undefined, undefined) : xs
        len = length xs
        arr = listArray ((0, 0), (len, maxW)) [foo i j | i <- [0..len], j <-[0..maxW]]
        foo 0 _ = 0
        foo i j | ws !! i > j = arr ! (i-1, j)
        foo i j = max (arr ! (i-1, j)) (arr ! (i-1, j - ws !! i) + vs !! i)
