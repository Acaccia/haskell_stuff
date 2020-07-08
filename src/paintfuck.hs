{-# LANGUAGE LambdaCase #-}
module Haskell.SylarDoom.Paintfuck where

import           Control.Monad.State.Strict
import Data.Array.ST
import           Text.ParserCombinators.Parsec hiding (State)

interpreter :: String -> Int -> Int -> Int -> String
interpreter code it w h = case parse parsePF "PF" code of Right commands -> evalState (exec commands) undefined
  where exec = undefined

data Command = North | South | East | West | Flip | Loop [Command]

parsePF :: Parser [Command]
parsePF = many (noneOf "<>*[]") *> many ((loopParse <|> simpleParse) <* many (noneOf "nsew*[]"))

loopParse :: Parser Command
loopParse = Loop <$> between (char '[') (char ']') parsePF

simpleParse :: Parser Command
simpleParse = North <$ char 'n'
          <|> South <$ char 's'
          <|> East  <$ char 'e'
          <|> West  <$ char 'w'
          <|> Flip  <$ char '*'

type Coord = (Int, Int)
type Grid s = STUArray s Coord Char
