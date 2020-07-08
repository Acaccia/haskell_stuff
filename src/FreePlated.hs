{-# language DeriveFunctor #-}
{-# language DeriveFoldable #-}
{-# language DeriveTraversable #-}
{-# language FlexibleInstances #-}
{-# language FunctionalDependencies #-}
{-# language MultiParamTypeClasses #-}
{-# language TemplateHaskell #-}

module FreePlated where

import Control.Lens.Fold   ((^?))
import Control.Lens.Plated (Plated, rewrite)
import Control.Lens.Prism  (aside)
import Control.Lens.Review ((#))
import Control.Lens.TH     (makeClassyPrisms)
import Control.Monad.Free  (Free, liftF, _Free)
import Data.Monoid         (First(..))

data InstF a
  = One a
  | Many Int a
  | Pause a
  deriving (Functor, Foldable, Traversable, Eq, Show)

type Inst = Free InstF

one :: Inst ()
one = liftF $ One ()

many :: Int -> Inst ()
many n = liftF $ Many n ()

pause :: Inst ()
pause = liftF $ Pause ()

makeClassyPrisms ''InstF
