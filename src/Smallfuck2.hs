{-# LANGUAGE LambdaCase #-}
module Haskell.SylarDoom.Smallfuck where

import           Control.Monad.ST.Strict       (runST)
import qualified Data.Vector.Unboxed           as V
import qualified Data.Vector.Unboxed.Mutable   as VM
import           Text.Parsec

type ParserSF = Parsec String (V.Vector Char, Int)

parseLeft, parseRight, parseLoop, parseFlip, parseSF :: ParserSF String

parseLeft = char '<' *> move pred

parseRight = char '>' *> move succ

parseFlip = char '*' *> flp

parseLoop = undefined

parseSF = parseLeft <|> parseRight <|> parseFlip <|> parseLoop

  -- (V.toList . fst <$> getState)

move :: (Int -> Int) -> ParserSF String
move f = do (vec, i) <- fmap f <$> getState
            if i < 0 || i >= V.length vec
              then putState (vec, i) >> parseSF
              else return (V.toList vec)

flp :: ParserSF String
flp = do (vec, i) <- getState
         let vec' = runST $ do
               mv <- V.unsafeThaw vec
               VM.unsafeModify mv (\b -> if b == '0' then '1' else '0') i
               V.unsafeFreeze mv
         putState (vec', i) >> parseSF

loop :: String -> ParserSF String
loop l = uncurry V.unsafeIndex <$> getState >>= \case
  '0' -> parseSF
  '1' -> undefined
