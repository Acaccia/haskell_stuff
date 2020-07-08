module ConvertCase where

import Data.Char       (isUpper, toLower, toUpper)
import Data.List       (foldl1', intercalate)
import Data.List.Split (keepDelimsL, split, splitWhen, whenElt)
import Text.Read       (readMaybe)

data Case = Camel | Snake | Kebab

instance Read Case where
  readsPrec _ "camel" = [(Camel, "")]
  readsPrec _ "snake" = [(Snake, "")]
  readsPrec _ "kebab" = [(Kebab, "")]
  readsPrec _ _       = []

caseType :: String -> Maybe Case
caseType xs = case ('_' `elem` xs, '-' `elem` xs) of
  (True, False)  -> Just Snake
  (False, True)  -> Just Kebab
  (False, False) -> Just Camel
  _              -> Nothing

splitCase :: Case -> String -> [String]
splitCase Camel = (split . keepDelimsL . whenElt) isUpper
splitCase Snake = splitWhen (== '_')
splitCase Kebab = splitWhen (== '-')

join :: Case -> [String] -> String
join _   [] = []
join cas xs = case cas of
    Camel -> foldl1' (\acc ys -> acc ++ upperize ys) xs
    Snake -> intercalate "_" (map lowerize xs)
    Kebab -> intercalate "-" (map lowerize xs)
  where lowerize (x:xs) = toLower x : xs
        upperize (x:xs) = toUpper x : xs

fromTo :: String -> Case -> Case -> String
fromTo xs from to = join to (splitCase from xs)

changeCase :: String -> String -> Maybe String
changeCase "" _  = Just ""
changeCase xs ys = fromTo xs <$> caseType xs <*> readMaybe ys
