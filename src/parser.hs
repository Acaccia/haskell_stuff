module Codewars.Kirilloid.MultilinearPolynomials where

import           Control.Arrow
import           Data.List
import           Data.Map.Strict               (fromAscListWith, toList)
import           Text.ParserCombinators.Parsec
import           Text.Printf

simplify :: String -> String
simplify str = case concatMap toString . reduce <$> parse parseExpr "expr" str of
  Right ('+' : expr) -> expr
  Right expr         -> expr

parseExpr :: Parser [(String, Int)]
parseExpr = many1 (try parseVar <|> parseNum)

parseVar :: Parser (String, Int)
parseVar = do
  neg <- elem '-' <$> many (oneOf "+-")
  num <- read <$> option "1" (many1 digit)
  var <- sort <$> many1 lower
  return (var, if neg then -num else num)

parseNum :: Parser (String, Int)
parseNum = do
  neg <- elem '-' <$> many (oneOf "+-")
  num <- read <$> many1 digit
  return ("", if neg then -num else num)

reduce :: [(String, Int)] -> [(String, Int)]
reduce = sortOn ((length &&& id) . fst) . toList . fromAscListWith (+) . sort

toString :: (String, Int) -> String
toString (var, 0)  = ""
toString (var, 1)  = '+' : var
toString (var, -1) = '-' : var
toString (var, n)  = printf "%+d%s" n var


test :: String
test = "10bcx-2adxz+byz+bdxyz-14acdxz-12dz-13acdz+7acdxyz-14acx+7bcdy-10abdxz+2by-2ayz-15cdx-10bcz+11bxy-12abdyz+11abcxyz-0bcz+4z+6bcdz-3bxz-6abcz+4abdyz+15bcd-13cdx+2bdy+12bcdyz+7bcxz+13abdy-13abdz-14cdy+8acdxy-13x+8xyz-10cx-12cdz-10abcxyz+15c+4bcxz+14a+7bdz-0bcx+8abcdx-12abcx-10z-0acd-8abcdxz+11bcdxz-7bdxyz-+7abdy"
