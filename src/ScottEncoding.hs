{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module ScottEncoding where

import Prelude hiding (concat, curry, filter, foldl, foldr, fst, length, map,
                null, snd, take, uncurry, zip)

newtype SMaybe a = SMaybe { runMaybe :: forall b. b -> (a -> b) -> b }
newtype SList a = SList { runList :: forall b. b -> (a -> SList a -> b) -> b }
newtype SEither a b = SEither { runEither :: forall c. (a -> c) -> (b -> c) -> c }
newtype SPair a b = SPair { runPair :: forall c. (a -> b -> c) -> c }

toMaybe :: SMaybe a -> Maybe a
toMaybe (SMaybe m) = m Nothing Just

fromMaybe :: Maybe a -> SMaybe a
fromMaybe Nothing  = SMaybe const
fromMaybe (Just a) = SMaybe (const ($ a))

isJust :: SMaybe a -> Bool
isJust (SMaybe m) = m False (const True)

isNothing :: SMaybe a -> Bool
isNothing (SMaybe m) = m True (const False)

catMaybes :: SList (SMaybe a) -> SList a
catMaybes = map fromJust . filter isJust
  where fromJust (SMaybe m) = m undefined id

toList :: SList a -> [a]
toList (SList l) = l [] (\x xs -> x : toList xs)

fromList :: [a] -> SList a
fromList []     = nil
fromList (x:xs) = SList (\_ f -> f x $ fromList xs)

empty :: SList a
empty = SList const

cons :: a -> SList a -> SList a
cons x sl = SList $ \_ f -> f x sl

concat :: SList a -> SList a -> SList a
concat sl1@(SList l) sl2 = l sl2 (\x xs -> x `cons` concat xs sl2)

null :: SList a -> Bool
null (SList l) = l True (\_ _ -> False)

length :: SList a -> Int
length (SList l) = l 0 (\_ xs -> 1 + length xs)

map :: (a -> b) -> SList a -> SList b
map f (SList l) = l empty (\x xs -> f x `cons` map f xs)

filter :: (a -> Bool) -> SList a -> SList a
filter p (SList l) = l empty (\x xs -> if p x then cons x (filter p xs) else filter p xs)

zip :: SList a -> SList b -> SList (SPair a b)
zip (SList l) (SList m) = l empty $ \x xs -> m empty $ \y ys -> fromPair (x, y) `cons` zip xs ys

foldl :: (b -> a -> b) -> b -> SList a -> b
foldl f acc (SList l) = l acc (\x xs -> foldl f (f acc x) xs)

foldr :: (a -> b -> b) -> b -> SList a -> b
foldr f acc (SList l) = l acc (\x xs -> f x $ foldr f acc xs)

take :: Int -> SList a -> SList a
take n _         | n <= 0 = SList const
take n (SList l) = l empty (\x xs -> cons x $ take (n-1) xs)

toEither :: SEither a b -> Either a b
toEither (SEither e) = e Left Right

fromEither :: Either a b -> SEither a b
fromEither (Left l)  = SEither (\f _ -> f l)
fromEither (Right r) = SEither (\_ f -> f r)

isLeft :: SEither a b -> Bool
isLeft (SEither e) = e (const True) (const False)

isRight :: SEither a b -> Bool
isRight (SEither e) = e (const False) (const True)

partition :: SList (SEither a b) -> SPair (SList a) (SList b)
partition = foldr part (fromPair (SList const, SList const))
  where part e = if isLeft e then first (cons $ fromLeft e) else second (cons $ fromRight e)
        fromLeft (SEither e) = e id undefined
        fromRight (SEither e) = e undefined id

toPair :: SPair a b -> (a,b)
toPair (SPair p) = p (,)

fromPair :: (a,b) -> SPair a b
fromPair (a, b) = SPair (\f -> f a b)

fst :: SPair a b -> a
fst (SPair p) = p const

snd :: SPair a b -> b
snd (SPair p) = p (flip const)

swap :: SPair a b -> SPair b a
swap (SPair p) = SPair (p . flip)

curry :: (SPair a b -> c) -> a -> b -> c
curry f a b = f $ fromPair (a, b)

uncurry :: (a -> b -> c) -> (SPair a b -> c)
uncurry f (SPair p) = p f

first :: (a -> b) -> SPair a c -> SPair b c
first f (SPair p) = p $ \a b -> fromPair (f a, b)

second :: (b -> c) -> SPair a b -> SPair a c
second f (SPair p) = p $ \a b -> fromPair (a, f b)
