{-# LANGUAGE GADTs                #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module TimesComm where


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

-- | Peano definition of multiplication.
type family (:*:) (n :: *) (m :: *) :: *
type instance Z :*: m = Z
type instance S n :*: m = m :+: (n :*: m)

-- | Addition for natural numbers.
plus :: Natural a -> Natural b -> Natural (a :+: b)
plus NumZ right        = right
plus (NumS left) right = NumS (left `plus` right)

-- | Multiplication for natural numbers.
mult :: Natural a -> Natural b -> Natural (a :*: b)
mult NumZ _            = NumZ
mult (NumS left) right = right `plus` (left `mult` right)

-- | For any n, n = n.
reflexive :: Natural n -> Equal n n
reflexive NumZ     = EqlZ
reflexive (NumS n) = EqlS (reflexive n)

-- | if a = b, then b = a.
symmetric :: Equal a b -> Equal b a
symmetric EqlZ     = EqlZ
symmetric (EqlS e) = EqlS (symmetric e)

-- | if a = b and b = c, then a = c.
transitive :: Equal a b -> Equal b c -> Equal a c
transitive EqlZ EqlZ           = EqlZ
transitive (EqlS ab) (EqlS bc) = EqlS (transitive ab bc)

-- | a + 0 = a
plusZeroRightNeutral :: Natural a -> Equal (a :+: Z) a
plusZeroRightNeutral NumZ     = EqlZ
plusZeroRightNeutral (NumS n) = EqlS (plusZeroRightNeutral n)

-- | (a + 1) + b = a + (b + 1)
plusSuccRightSucc :: Natural a -> Natural b -> Equal (S a :+: b) (a :+: S b)
plusSuccRightSucc NumZ right        = reflexive (NumS right)
plusSuccRightSucc (NumS left) right = EqlS (plusSuccRightSucc left right)

-- | a + b = b + a
plusCommutes :: Natural a -> Natural b -> Equal (a :+: b) (b :+: a)
plusCommutes NumZ right = symmetric (plusZeroRightNeutral right)
plusCommutes (NumS left) right = EqlS (plusCommutes left right) `transitive` plusSuccRightSucc right left

-- | a + (b + c) = (a + b) + c
plusAssoc :: Natural a -> Natural b -> Natural c -> Equal (a :+: (b :+: c)) ((a :+: b) :+: c)
plusAssoc NumZ centre right        = reflexive (centre `plus` right)
plusAssoc (NumS left) centre right = EqlS (plusAssoc left centre right)

-- | a * 0 = 0
zeroComm :: Natural a -> Equal (a :*: Z) Z
zeroComm NumZ     = EqlZ
zeroComm (NumS n) = zeroComm n

multRightSuccPlus :: Natural a -> Natural b -> Equal (a :*: S b) (a :+: (a :*: b))
multRightSuccPlus = undefined


multLeftSuccPlus :: Natural a -> Natural b -> Equal (S a :*: b) (b :+: (a :*: b))
multLeftSuccPlus left right = reflexive (NumS left `mult` right)

-- This is the proof that the kata requires.
-- | a * b = b * a
timesComm :: Natural a -> Natural b -> Equal (a :*: b) (b :*: a)
timesComm NumZ right        = symmetric (zeroComm right)
timesComm (NumS left) right = (reflexive $ NumS left `mult` right) `transitive` symmetric (multRightSuccPlus right left)
