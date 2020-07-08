module JSON.Parser where

import           Text.ParserCombinators.Parsec hiding (parse)
import qualified Text.ParserCombinators.Parsec as P

data Value = String String
           | Number Double
           | Object [(Value,Value)] -- an association list -- only a `String` is valid as the index `Value`
           | Array [Value]          -- not limited to identical primitive datatypes
           | Boolean Bool           -- either `True` or `False`
           | Null
           deriving Show

parse :: String -> Maybe Value
parse = either (const Nothing) Just . P.parse (parseValue <* eof) "JSON"

parseValue :: Parser Value
parseValue = try parseObject <|> try parseArray <|> try parseNull <|> try parseBoolean <|> try parseNumber <|> parseString

parseString :: Parser Value
parseString = String <$> between (char '"') (char '"') (many (noneOf "\""))

parseNumber :: Parser Value
parseNumber = Number <$> do
  s <- string "-" <|> pure ""
  n <- ((:) <$> oneOf "123456789" <*> many digit) <|> string "0"
  f <- (char '.' >> many1 digit) <|> pure "0"
  return (read $ s ++ n ++ "." ++ f)

parseBoolean :: Parser Value
parseBoolean = Boolean <$> (True <$ string "true" <|> False <$ string "false")

parseNull :: Parser Value
parseNull = Null <$ string "null"

parseArray :: Parser Value
parseArray = Array <$> between (char '[') (char ']') (tok parseValue `sepBy` char ',')

parseObject :: Parser Value
parseObject = Object <$> between (char '{') (char '}') (((,) <$> tok parseString <*> (char ':' *> tok parseValue)) `sepBy` char ',')

tok :: Parser a -> Parser a
tok a = spaces *> a <* spaces
