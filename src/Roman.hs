module Roman where

data Roman = I | V | X | L | C | D | M
  deriving (Eq, Ord, Read, Show)

charToRoman :: Char -> Roman
charToRoman = read . pure

romanToInt :: Roman -> Int
romanToInt I = 1
romanToInt V = 5
romanToInt X = 10
romanToInt L = 50
romanToInt C = 100
romanToInt D = 500
romanToInt M = 1000

solution :: String -> Int
solution = go . map charToRoman where
  go [] = 0
  go [x] = romanToInt x
  go (x:y:xs) = if x < y
    then (romanToInt y - romanToInt x) + go xs
    else romanToInt x + go (y:xs)
