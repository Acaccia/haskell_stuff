module Calculator where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token

main :: IO ()
main = interact evaluate

evaluate :: String -> String
evaluate = either undefined (show . (`mod` 1000000007) . round) . parse parseExpr "Calculator" where
  parseExpr = buildExpressionParser operators (parens lexer parseExpr <|> (either fromInteger id <$> naturalOrFloat lexer))
  lexer = makeTokenParser $ emptyDef { opStart = oneOf "+-*/", opLetter = oneOf "+-*/" }
  operators = [ [ unary "+" id, unary "-" negate]
              , [ binary "*" (*), binary "/" (/) ]
              , [ binary "+" (+), binary "-" (-) ]
              ] where binary s f = Infix (f <$ symbol lexer s) AssocRight
                      unary s f = Prefix (f <$ symbol lexer s)

{-
lexer :: TokenParser ()
lexer = makeTokenParser $ emptyDef { opStart = oneOf "+-*/", opLetter = oneOf "+-*/" }

operators :: OperatorTable Char () Double
operators = [ [ unary "+" id, unary "-" negate]
            , [ binary "*" (*), binary "/" (/) ]
            , [ binary "+" (+), binary "-" (-) ]
            ]
  where binary s f = Infix (f <$ symbol lexer s) AssocRight
        unary s f = Prefix (f <$ symbol lexer s)

parseExpr :: Parser Double
parseExpr = buildExpressionParser operators (parens lexer parseExpr <|> (either fromInteger id <$> naturalOrFloat lexer))
--}
