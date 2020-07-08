{-# LANGUAGE GADTs         #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Kata.AdditionCommutes
  ( plusCommutes ) where

-- These are some lemmas that may be helpful.
-- They will *not* be tested, so rename them
-- if you so desire. Good luck!

-- | For any n, n = n.
reflexive :: Natural n -> Equal n n
reflexive NumZ = EqlZ
reflexive (NumS n) = EqlS (reflexive n)

-- | if a = b, then b = a.
symmetric :: Equal a b -> Equal b a
symmetric EqlZ = EqlZ
symmetric (EqlS e) = EqlS (symmetric e)

-- | if a = b and b = c, then a = c.
transitive :: Equal a b -> Equal b c -> Equal a c
transitive EqlZ EqlZ = EqlZ
transitive (EqlS ab) (EqlS bc) = EqlS (transitive ab bc)

-- | a + 0 = a
plusZeroRightNeutral :: Natural a -> Equal (a :+: Z) a
plusZeroRightNeutral NumZ = EqlZ
plusZeroRightNeutral (NumS n) = EqlS (plusZeroRightNeutral n)

-- | (a + 1) + b = a + (b + 1)
plusSuccRightSucc :: Natural a -> Natural b -> Equal (S a :+: b) (a :+: S b)
plusSuccRightSucc NumZ n = reflexive (NumS n)
plusSuccRightSucc (NumS m) n = EqlS (plusSuccRightSucc m n)

-- | a + b = b + a
plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusCommutes NumZ b = symmetric (plusZeroRightNeutral b)
plusCommutes (NumS a) b = transitive (EqlS $ plusCommutes a b) (plusSuccRightSucc b a)

-- For reference, here are the definitions, if you
-- want to copy them into an IDE:

-- | The natural numbers, encoded in types.
data Z
data S n

-- | Predicate describing natural numbers.
-- | This allows us to reason with `Nat`s.
data Natural :: * -> * where
    NumZ :: Natural Z
    NumS :: Natural n -> Natural (S n)

-- | Predicate describing equality of natural numbers.
data Equal :: * -> * -> * where
    EqlZ :: Equal Z Z
    EqlS :: Equal n m -> Equal (S n) (S m)

-- | Peano definition of addition.
type family (:+:) (n :: *) (m :: *) :: *
type instance Z :+: m = m
type instance S n :+: m = S (n :+: m)
