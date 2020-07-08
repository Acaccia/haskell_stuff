module Kata.KnightTour where
import Control.Monad (guard)
import Data.Maybe    (listToMaybe)
import Data.Set      (empty, insert, notMember, singleton, size)

type Dim = (Int,Int) -- Dimensions of the chessboard (width,height)
type Pos = (Int,Int) -- Position (x,y), 1 based, (1,1) being top left corner
type Solution = [Pos] -- Solution is a list of positions

solve :: Dim -> Pos -> Maybe Solution
solve (w,h) initial | not (inBoard initial) = Nothing
                    | otherwise = listToMaybe $ go initial (singleton initial)
  where
    moves (x, y) = [(x-2, y-1), (x-2, y+1), (x-1, y-2), (x-1, y+2), (x+1, y-2), (x+1, y+2), (x+2, y-1), (x+2, y+1)]
    inBoard (x, y) = x > 0 && x <= w && y > 0 && y <= h
    go pos visited = if size visited == w * h then return [pos] else do
      newPos <- filter (\x -> inBoard x && x `notMember` visited) (moves pos)
      nxt <- go newPos (insert newPos visited)
      return (pos : nxt)
