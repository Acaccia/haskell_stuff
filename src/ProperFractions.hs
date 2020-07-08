{-# OPTIONS_GHC -rtsopts -threaded -O2 -with-rtsopts=-N #-}
module ProperFractions.JorgeVS.Kata where

import Control.Parallel.Strategies

properFractions :: Integer -> Integer
properFractions  = toInteger . length . filter (== 1) . (parMap rpar . gcd <*> enumFromTo 1)
