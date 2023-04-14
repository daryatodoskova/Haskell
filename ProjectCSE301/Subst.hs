module Subst (subst) where

import Expr

-- define a new string that comes lexicographically later than all of them
-- (a->b, b->c, ..., y->z, z->a', a'->b',...,z'->a'',...)
fresh :: [Var] -> Var
fresh xs = head [x | x <- map (:[]) ['a'..'z'] ++ map ((++"'") . (:[])) ['a'..'z'], x `notElem` xs]

subst :: (LExp,Var) -> LExp -> LExp
subst (t,x) (V y)     = if x == y then t else (V y)
subst (t,x) (A t1 t2) = A (subst (t,x) t1) (subst (t,x) t2)
subst (t, x) (L y t1)
  | x == y = L y t1
  | y `elem` free t = let z = fresh (free t) in L z (subst (t, x) (swapname (y, z) t1))
  | otherwise = L y (subst (t, x) t1)
