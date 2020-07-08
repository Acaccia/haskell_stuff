{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}
module PolyvariadicFunctions where

-- `polyAdd` sums its arguments, all `Int`s.
class PolyAdd r where
  polyAdd' :: Int -> r

instance (PolyAdd r, a ~ Int) => PolyAdd (a -> r) where
  polyAdd' x y = polyAdd' (x + y)

instance PolyAdd Int where
  polyAdd' = id

polyAdd :: PolyAdd r => r
polyAdd = polyAdd' 0

-- `polyList` turns its arguments into a list, polymorphically.
class PolyList a r | r -> a where
  polyList' :: [a] -> r

instance (PolyList a r) => PolyList a (a -> r) where
  polyList' xs x = polyList' (xs ++ [x])

instance PolyList a [a] where
  polyList' = id

polyList :: PolyList a r => r
polyList = polyList' []

-- `polyWords` turns its arguments into a spaced string.
class PolyWords r where
  polyWords' :: String -> r

instance PolyWords r => PolyWords (String -> r) where
  polyWords' "" ys = polyWords' ys
  polyWords' xs ys = polyWords' (xs ++ " " ++ ys)

instance PolyWords String where
  polyWords' = id

polyWords :: PolyWords r => r
polyWords = polyWords' ""
