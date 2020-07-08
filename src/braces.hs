module Braces where

import Text.ParserCombinators.Parsec
import Control.Applicative (liftA2)

data Curvy = Str String | Curve [[Curvy]] deriving Show


parseCurvy :: Parser [Curvy]
parseCurvy = many1 (parseCurve <|> parseStr)

parseCurve, parseStr, parseStrNoComma :: Parser Curvy
parseCurve = Curve <$> between (char '{') (char '}') (many1 (parseCurve <|> parseStrNoComma) `sepBy` char ',')
parseStr = Str <$> many1 (noneOf "{}")
parseStrNoComma = Str <$> (many1 (noneOf "{},") <|> (try (char ',') *> pure ""))

eval :: [Curvy] -> [String]
eval = foldr (liftA2 (++) . foo) [""]
  where foo (Str s) = [s]
        foo (Curve [cs]) = eval cs
