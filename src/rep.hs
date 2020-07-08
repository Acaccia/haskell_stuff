import Data.List (tails)

import Control.Monad
import Debug.Trace

solve :: String -> String -> Int
solve = undefined

go [] _  = mzero
go _  [] = mzero
go [x] (y:ys) = (if x == y then [()] else mzero) `mplus` go [x] ys
go xs ys = (when (head xs == head ys) (go (tail xs) (tail ys))) `mplus` go xs (tail ys)
