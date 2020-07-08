module StringExpansion where

import Text.ParserCombinators.ReadP
import Data.Char

solve :: [Char] -> [Char]
solve xs = head [x | (x, "") <- readP_to_S parseExpansion xs]

parseExpansion = concat <$> many (parseMult +++ many1 (satisfy isAlpha))

parseMult = concatRepl <$> option 1 (read <$> many1 (satisfy isDigit)) <*> between (char '(') (char ')') parseExpansion

concatRepl n = concat . replicate n
