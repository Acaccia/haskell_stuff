
import Prelude hiding (sequence, mapM)
import Control.Applicative

sequence :: Applicative f => [f a] -> f [a]
sequence = foldr (liftA2 (:)) (pure [])

mapM :: Applicative f => (a -> f b) -> [a] -> f [b]
mapM f = sequence . map f

foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM f = foldl (\a x -> a >>= \b -> f b x) . pure
