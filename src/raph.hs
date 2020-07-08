module Rahp where

import Text.ParserCombinators.Parsec

type Raph = [(String, Int)]

minusParser :: Parser String
minusParser = many1 lower

intParser :: Parser Int
intParser = fmap read (many1 digit)

lineParser :: Parser (String, Int)
lineParser = do
  m <- minusParser
  space
  n <- intParser
  return (m, n)

lineParser' :: Parser (String, Int)
lineParser' = (,) <$> minusParser <*> (spaces *> intParser)

raphParser :: Parser Raph
raphParser = endBy1 lineParser eol

eol = newline
