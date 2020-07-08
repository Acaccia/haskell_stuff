module Lookandsay where

import           Control.Monad
import           Data.Array    (Array, listArray, (!))
import           Data.Char     (digitToInt, intToDigit)
import           Data.List     (group)

las :: Array Int String
las = listArray (1, 55) (map go [1..55])
  where go 1 = "1"
        go n = say $ las ! (n - 1)
        say = group >=> liftM2 ((. pure) . (:)) (intToDigit . length) head
