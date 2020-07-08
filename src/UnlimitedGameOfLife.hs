module UnlimitedGameOfLife where
import qualified Data.Vector as V
import Control.Comonad

getGeneration :: [[Int]] -> Int -> [[Int]]
getGeneration = undefined

data Cell = Dead | Alive deriving (Enum, Eq, Show)

newtype V2 a = V2 (V.Vector (V.Vector a)) deriving Show

instance Functor V2 where
  fmap f (V2 v) = V2 (V.map (V.map f) v)

data CV2 a = CV2 (V2 a) (Int, Int) deriving Show

instance Functor CV2 where
  fmap f (CV2 v i) = CV2 (fmap f v) i

instance Comonad CV2 where
  extract (CV2 (V2 v) (i, j)) = v V.! i V.! j
  extend f (CV2 (V2 v) i) = undefined
