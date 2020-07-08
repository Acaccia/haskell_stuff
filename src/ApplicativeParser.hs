module ApplicativeParser where

import           Data.Char
import           Prelude   hiding (fmap)

-- | An ambiguous parser.
newtype Parser a = P { unP :: String -> [(String, a)] }

-- | Change the result of a parser.
pmap :: (a -> b) -> Parser a -> Parser b
pmap f (P p) = P $ \k -> [(str, f a) | (str, a) <- p k]

-- | Operator version of 'pmap'.
(<#>) :: (a -> b) -> Parser a -> Parser b
(<#>) = pmap

-- | Parse a value and replace it.
(<#) :: a -> Parser b -> Parser a
(<#) = pmap . const

infixl 4 <#>
infixl 4 <#

-- | Parse a character only when a predicate matches.
predP :: (Char -> Bool) -> Parser Char
predP p = P $ \k -> case k of
  []     -> []
  (c:cs) -> if p c then [(cs, c)] else []

-- | Succeed only when parsing the given character.
charP :: Char -> Parser Char
charP = predP . (==)

tokP :: Char -> Parser Char
tokP c = charP c <@ charP ' '

-- | Inject a value into an identity parser.
inject :: a -> Parser a
inject x = P $ \k -> [(k, x)]

-- | Given a parser with a function value and another parser, parse the function
-- first and then the value, return a parser which applies the function to the
-- value.
(<@>) :: Parser (a -> b) -> Parser a -> Parser b
P pf <@> P px = P $ \k -> [(str', f a) | (str, f) <- pf k, (str', a) <- px str]

lift2 :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
lift2 f a b = pmap f a <@> b

(<@) :: Parser a -> Parser b -> Parser a
(<@)= lift2 const

(@>) :: Parser a -> Parser b -> Parser b
(@>)= lift2 (const id)

infixl 4 <@
infixl 4 @>
infixl 4 <@>

-- | Parse a whole string.
stringP :: String -> Parser String
stringP = foldr (lift2 (:) . charP) (inject "")

-- | Construct a parser that never parses anything.
emptyP :: Parser a
emptyP = P $ const []

-- | Combine two parsers: When given an input, provide the results of both parser run on the input.
(<<>>) :: Parser a -> Parser a -> Parser a
p <<>> q = P $ \k -> unP p k ++ unP q k

(|||) :: Parser a -> Parser a -> Parser a
p ||| q = P $ \k -> case unP p k of
  []  -> unP q k
  res -> res

infixl 3 <<>>
infixl 3 |||

-- | Apply the parser zero or more times.
many :: Parser a -> Parser [a]
many p = some p <<>> inject []

-- | Apply the parser one or more times.
some :: Parser a -> Parser [a]
some p = lift2 (:) p (many p)


-- | Apply a parser and return all ambiguous results, but only those where the input was fully consumed.
runParser :: Parser a -> String -> [a]
runParser p cs = foldr cons [] (unP p cs)
  where cons ("", a) acc = a : acc
        cons _       acc = acc

-- | Apply a parser and only return a result, if there was only one unambiguous result with output fully consumed.
runParserUnique :: Parser a -> String -> Maybe a
runParserUnique p cs = case unP p cs of
  [("", a)] -> Just a
  _         -> Nothing

-- | Kinds of binary operators.
data BinOp = AddBO | MulBO deriving (Eq, Show)

-- | Some kind of arithmetic expression.
data Expr = ConstE Int
          | BinOpE BinOp Expr Expr
          | NegE Expr
          | ZeroE
          deriving (Eq, Show)

evalExpr :: Expr -> Int
evalExpr (ConstE i)         = i
evalExpr (BinOpE AddBO a b) = evalExpr a + evalExpr b
evalExpr (BinOpE MulBO a b) = evalExpr a * evalExpr b
evalExpr (NegE a)           = negate (evalExpr a)
evalExpr ZeroE              = 0

-- | Parse arithmetic expressions, with the following grammar:
--
--     expr         ::= const | binOpExpr | neg | zero
--     const        ::= int
--     binOpExpr    ::= '(' expr ' ' binOp ' ' expr ')'
--     binOp        ::= '+' | '*'
--     neg          ::= '-' expr
--     zero         ::= 'z'
--
parseExpr :: String -> Maybe Expr
parseExpr = runParserUnique pExpr

pAdd, pConst, pExpr, pMul, pNeg, pZero :: Parser Expr
pAdd = lift2 (BinOpE AddBO) (charP '(' @> pExpr <@ stringP " + ") (pExpr <@ charP ')')
pConst = pmap (ConstE . digitToInt) (predP isDigit)
pExpr = pMul <<>> pAdd <<>> pNeg <<>> pZero <<>> pConst
pMul = lift2 (BinOpE MulBO) (charP '(' @> pExpr <@ stringP " * ") (pExpr <@ charP ')')
pNeg = charP '-' @> pmap NegE pExpr
pZero = charP 'z' @> inject ZeroE
