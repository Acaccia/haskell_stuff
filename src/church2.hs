module Haskell.Codewars.Church where

type Lambda a = (a -> a)
type Cnum a = Lambda a -> Lambda a

churchAdd :: Cnum a -> Cnum a -> Cnum a
churchAdd c1 = c1 churchSucc where churchSucc c = (\h -> h . c h)

churchMul :: Cnum a -> Cnum a -> Cnum a
churchMul c1 c2 = c1 (churchAdd c2) zero
  where zero f = id

--Extra credit: Why is the type signature different?
churchPow :: Cnum a -> (Cnum a -> Cnum a) -> Cnum a
churchPow cb ce = undefined
