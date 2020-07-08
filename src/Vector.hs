{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Vector where

import GHC.TypeLits
import Data.Proxy
import Data.Foldable

data Vector :: Nat -> * -> * where
  Nil :: Vector 0 a
  (:-) :: a -> Vector n a -> Vector (succ n) a

instance Foldable (Vector n) where
  foldMap _ Nil = mempty
  foldMap f (x :- xs) = f x `mappend` foldMap f xs

  foldr _ acc Nil = acc
  foldr f acc (x :- xs) = x `f` foldr f acc xs

instance Functor (Vector n) where
  fmap _ Nil = Nil
  fmap f (x :- xs) = f x :- fmap f xs

size :: forall a n num. (KnownNat n, Num num) => Vector n a -> num
size _ = fromInteger $ natVal (Proxy :: Proxy n)
