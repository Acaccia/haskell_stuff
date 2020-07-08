{-# LANGUAGE DeriveFunctor, LambdaCase #-}
module Coroutine where

import Control.Monad

data Command r u d a =
    Done a
  | Out d (Coroutine r u d a)
  | In (u -> Coroutine r u d a)
  deriving (Functor)

newtype Coroutine r u d a = Coroutine { runCoroutine :: (Command r u d a -> r) -> r } deriving (Functor)

-- Useful alias
apply :: Coroutine r u d a -> (Command r u d a -> r) -> r
apply = runCoroutine

instance Applicative (Coroutine r u d) where
  pure a = Coroutine ($ Done a)
  (<*>) = ap

instance Monad (Coroutine r u d) where
  return a = Coroutine ($ Done a)
  f >>= g  = Coroutine $ \k -> apply f $ \case
    Done x -> g x `apply` k
    Out d c -> k $ Out d (c >>= g)
    In fn -> k $ In (fn >=> g)


(>>>) :: Coroutine r u m a -> Coroutine r m d a -> Coroutine r u d a
p1 >>> p2 = Coroutine $ \k -> apply p2 $ \case
  Done x -> k $ Done x
  Out d c -> k $ Out d (p1 >>> c)
  In fn -> apply p1 $ \case
    Done x -> k $ Done x
    Out d c -> apply (c >>> fn d) k
    In fn' -> k $ In (\k' -> fn' k' >>> p2)

-- It might be useful to define the following function
-- pipe2 :: Coroutine r u m a -> (m -> Coroutine r m d a) -> Coroutine r u d a

-- Library functions

output :: a -> Coroutine r u a ()
output v = Coroutine ($ Out v (return ()))

input :: Coroutine r v d v
input = Coroutine ($ In return)

produce :: [a] -> Coroutine r u a ()
produce = mapM_ output

consume :: Coroutine [t] u t a -> [t]
consume c = apply c $ \case
  Out d cor -> d : consume cor
  _         -> []

filterC :: (v -> Bool) -> Coroutine r v v ()
filterC p = forever $ input >>= liftM2 when p output

limit :: Int -> Coroutine r v v ()
limit n = replicateM_ n (input >>= output)

suppress :: Int -> Coroutine r v v ()
suppress n = replicateM_ n input >> forever (input >>= output)

add :: Coroutine r Int Int ()
add = forever $ liftM2 (+) input input >>= output

duplicate :: Coroutine r v v ()
duplicate = forever $ input >>= liftM2 (>>) output output

-- Programs
-- 1. A program which outputs the first 5 even numbers of a stream.
-- 2. A program which produces a stream of the triangle numbers 
-- 3. A program which multiplies a stream by 2
-- 4. A program which sums adjacent pairs of integers

p1, p2, p3, p4 :: Coroutine r Int Int ()

p1 = filterC even >>> limit 5
p2 = produce $ scanl1 (+) [1..]
p3 = duplicate >>> add
p4 = duplicate >>> suppress 1 >>> add
