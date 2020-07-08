{-# LANGUAGE LambdaCase #-}
module Haskell.SylarDoom.Smallfuck where

import           Control.Monad.ST.Strict       (runST)
import           Control.Monad.State.Strict
import qualified Data.Vector.Unboxed           as V
import qualified Data.Vector.Unboxed.Mutable   as VM
import           Text.ParserCombinators.Parsec hiding (State)

interpreter :: String -> String -> String
interpreter code tape = case parse parseSF "SF" code of Right coms -> evalState (execute coms) (V.fromList tape, 0)
  where execute :: [Command] -> State (V.Vector Char, Int) String
        execute (c : cs) = case c of
          MoveLeft  -> move pred cs
          MoveRight -> move succ cs
          Flip      -> flip cs
          Loop ls   -> loop ls cs
        execute []       = gets (V.toList . fst)
        move f cs = do (vec, i) <- fmap f <$> get
                       if i < 0 || i >= V.length vec
                         then return $ V.toList vec
                         else put (vec, i) >> execute cs
        flip cs = do (vec, i) <- get
                     let vec' = runST $ do mv <- V.unsafeThaw vec
                                           VM.modify mv (\b -> if b == '0' then '1' else '0') i
                                           V.unsafeFreeze mv
                     put (vec', i) >> execute cs
        loop ls cs = gets (uncurry V.unsafeIndex) >>= \case
          '0' -> execute cs
          '1' -> execute ls >> loop ls cs

data Command = MoveLeft | MoveRight | Flip | Loop [Command] deriving Show

parseSF :: Parser [Command]
parseSF = many (noneOf "<>*[]") *> many ((loopParse <|> simpleParse) <* many (noneOf "<>*[]"))

loopParse :: Parser Command
loopParse = Loop <$> between (char '[') (char ']') parseSF

simpleParse :: Parser Command
simpleParse = com <$> oneOf "><*" where
  com '>' = MoveRight
  com '<' = MoveLeft
  com '*' = Flip
