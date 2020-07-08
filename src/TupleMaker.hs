module Kata.TupleMaker (tuple) where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Monad

-- | Creates a lambda that takes `n` arguments and
-- | returns an n-tuple of those arguments.
tuple :: Int -> Q Exp
tuple n = liftM2 LamE (fmap VarP) (TupE . fmap VarE) <$> replicateM n (newName "x")

tuplee :: Int -> Q Exp
tuplee n = do
  xs <- replicateM n (newName "x")
  return $ LamE (VarP <$> xs) (TupE $ VarE <$> xs)
