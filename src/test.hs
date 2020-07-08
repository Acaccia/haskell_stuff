{-# LANGUAGE TemplateHaskell #-}
import Control.Lens
import Data.Char     (isDigit, isLower, isUpper)
import Data.Foldable (foldr')

data Counter i = Counter { _upper :: i, _lower :: i, _number :: i, _special :: i }

makeLenses ''Counter

instance Foldable Counter where

toList :: Counter i -> [i]
toList (Counter u l n s) = [u, l, n, s]

solve :: [Char] -> [Int]
solve = toList . foldr' count (Counter 0 0 0 0)
  where count x | isUpper x = upper +~ 1
                | isLower x = lower +~ 1
                | isDigit x = number +~ 1
                | otherwise = special +~ 1
