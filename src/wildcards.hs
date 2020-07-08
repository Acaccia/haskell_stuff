module Wildcards where

possibilities :: String -> [String]
possibilities = foldr go [""] where
  go '?' = concatMap (\x -> ['0':x, '1':x])
  go n   = map (n:)
