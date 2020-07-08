module Stream where

import Control.Applicative
import Control.Arrow

data Stream a = a :> Stream a
infixr :>

-- | Get the first element of a stream.
headS :: Stream a -> a
headS (h :> _) = h

-- | Drop the first element of a stream.
tailS :: Stream a -> Stream a
tailS (_ :> t) = t


-- {{{ Stream constructors

-- | Construct a stream by repeating a value.
repeatS :: a -> Stream a
repeatS a = a :> repeatS a

-- | Construct a stream by repeatedly applying a function.
iterateS :: (a -> a) -> a -> Stream a
iterateS f x = x :> iterateS f (f x)

-- | Construct a stream by repeating a list forever.
cycleS :: [a] -> Stream a
cycleS = fromList . cycle
  where fromList (x : xs) = x :> fromList xs

-- | Construct a stream by counting numbers starting from a given one.
fromS :: Num a => a -> Stream a
fromS = iterateS (+ 1)

-- | Same as 'fromS', but count with a given step width.
fromStepS :: Num a => a -> a -> Stream a
fromStepS x s = iterateS (+ s) x

-- }}}


-- | Fold a stream from the left.
foldrS :: (a -> b -> b) -> Stream a -> b
foldrS f (x :> xs) = x `f` foldrS f xs

-- | Filter a stream with a predicate.
filterS :: (a -> Bool) -> Stream a -> Stream a
filterS p = foldrS (\x -> if p x then (x :>) else id)

-- | Take a given amount of elements from a stream.
takeS :: Int -> Stream a -> [a]
takeS i (x :> xs) | i <= 0    = []
                  | otherwise = x : takeS (i - 1) xs

-- | Drop a given amount of elements from a stream.
dropS :: Int -> Stream a -> Stream a
dropS i s | i <= 0    = s
          | otherwise = dropS (i - 1) (tailS s)

-- | Do take and drop simultaneous.
splitAtS :: Int -> Stream a -> ([a], Stream a)
splitAtS i s | i <= 0    = ([], s)
             | otherwise = (headS s :) `first` splitAtS (i - 1) (tailS s)

-- | Combine two streams with a function.
zipWithS :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWithS f (x :> xs) (y :> ys) = f x y :> zipWithS f xs ys

zipS :: Stream a -> Stream b -> Stream (a, b)
zipS = zipWithS (,)

instance Functor Stream where
    -- fmap :: (a -> b) -> Stream a -> Stream b
    fmap f (x :> xs) = f x :> fmap f xs

instance Applicative Stream where
    -- pure :: a -> Stream a
    pure = repeatS

    -- (<*>) :: Stream (a -> b) -> Stream a -> Stream b
    (<*>) = zipWithS ($)

-- | The stream of fibonacci numbers.
fibS :: Stream Integer
fibS = 0 :> 1 :> zipWithS (+) fibS (tailS fibS)

-- | The stream of prime numbers.
primeS :: Stream Integer
primeS = 2 :> filterPrime [3,5..]
  where filterPrime (p : xs) = p :> filterPrime [x | x <- xs, x `mod` p /= 0]
