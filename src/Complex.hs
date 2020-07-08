{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}
module Complex where
import Data.Char

tidyNumber :: Int -> Bool
tidyNumber = all id . (zipWith (<=) <*> tail) . map digitToInt . show
