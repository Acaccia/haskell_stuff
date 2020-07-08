{-# LANGUAGE DeriveFunctor #-}

module OperatorParser
    ( OpTree(..)
    , Associativity(..)
    , op
    , foldTree
    , parseOperators
    , module Text.ParserCombinators.ReadP
    ) 
where

import Text.ParserCombinators.ReadP
import Control.Applicative 

-- | Type for operator parse results. 'a' is the type of the operator, 'b'
-- | of the terms.
data OpTree a b = Op (OpTree a b) a (OpTree a b) 
                | Term b 
                deriving (Show, Eq, Functor)

-- | Type for specifying the assocativity of operators: left, right, or none.
data Associativity a = L a | R a | NoAssociativity a
                     deriving (Show, Eq, Functor)

-- | Transform an OpTree using the given function.
foldTree :: (a -> b -> b -> b) -> OpTree a b -> b
foldTree _ (Term x)   = x
foldTree f (Op x a y) = f a (foldTree f x) (foldTree f y)  

-- | Return a parser such that: given 'op s a', if s matches, the parser 
-- | returns a.
op :: String -> a -> ReadP a
op s a = string s *> pure a

-- | Accept two arguments: 
-- | (1) A list of type [Associativity [ReadP a]], which contains parsers for
-- | operators (ReadP a). Each item of type Associativity [ReadP a] contains
-- | a group of operator parsers of the same precedence and associativity; 
-- | these groups are listed in order of precedence (lowest to highest).
-- | (2) A parser for the terms.
-- | And return a parser for operator expressions that yields a parse tree. 
parseOperators :: [Associativity [ReadP a]] -> ReadP b -> ReadP (OpTree a b)
parseOperators ops p = 
    foldr parseSamePrec ((Term <$> p) +++ brackets) ops
  where
    brackets = char '(' *> betweenSpaces (parseOperators ops p) <* char ')'

betweenSpaces :: ReadP a -> ReadP a
betweenSpaces p = skipSpaces *> p <* skipSpaces

parseSamePrec :: 
    Associativity [ReadP a] -> ReadP (OpTree a b) -> ReadP (OpTree a b)
parseSamePrec operators parser = 
    let opParser ops = betweenSpaces . choice $ map (flip Op <$>) ops
    in  case operators of
            L ops -> chainl1 parser (opParser ops)
            R ops -> chainr1 parser (opParser ops)
            NoAssociativity ops -> do
                parsed <- pure <$> parser
                (opParser ops <*> parsed <*> parser) <++ parsed
