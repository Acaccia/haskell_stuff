module TimesThreeOrPlusFive where
import Control.Monad

data Operation = One | TimesThree Operation | PlusFive Operation

instance Show Operation where
  show One = "1"
  show (TimesThree op) = '(' : show op ++ ") * 3"
  show (PlusFive op) = '(' : show op ++ ") + 5"

solutions :: Int -> Int -> [Int]
solutions limit acc = do
  guard $ acc < limit
  return 1


solve :: Int -> Maybe String
solve = undefined
