module SimpleTokenizer (Token(..), tokenise) where
import           Text.ParserCombinators.Parsec

data Token = Token String | Brackets [Token]
  deriving (Eq, Show)

tokenise :: String -> Maybe [Token]
tokenise = either (const Nothing) Just . parse (tokenizer <* spaces <* eof) "token"

operatorChars :: String
operatorChars = "!#$%&*+-/<=>@^_.,;"

tokenizer :: Parser [Token]
tokenizer = many (spaces *> (simpleAlpha <|> simpleOperator <|> brackets) <* spaces)

simpleAlpha :: Parser Token
simpleAlpha = Token <$> many1 letter

simpleOperator :: Parser Token
simpleOperator = Token <$> many1 (oneOf operatorChars)

brackets :: Parser Token
brackets = Brackets <$> between (char '(') (char ')') tokenizer
