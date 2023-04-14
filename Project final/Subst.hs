module Subst (subst) where

import Expr

fresh :: [Var] -> Var
fresh xs = case [x | x <- map (:[]) ['a'..'z'] ++ map ((++"'") . (:[])) ['a'..'z'], x `notElem` xs] of
             [] -> error "no fresh variable"
             (x:_) -> x

  

subst :: (LExp,Var) -> LExp -> LExp
subst (t,x) (V y)     = if x == y then t else (V y)
subst (t,x) (A t1 t2) = A (subst (t,x) t1) (subst (t,x) t2)
subst (t, x) (L y t1)
  | x == y = L y t1
  | y `elem` free t = let z = fresh (free t) in L z (subst (t, x) (swapname (y, z) t1))
  | otherwise = L y (subst (t, x) t1)



-- tests

-- if the bound variable is free in the substitution, then it is renamed
test1 = subst (V "x", "y") (V "y") == V "x"
-- if the bound variable is not free in the substitution, then it is not renamed
test2 = subst (V "x", "y") (V "z") == V "z"
-- if we have an application, then we substitute in both subexpressions
test3 = subst (V "x", "y") (A (V "y") (V "z")) == A (V "x") (V "z")
-- in the lambda we have to be careful to avoid capture
-- case 1. the substitution is the same as the bound variable, then we don't rename
test4 = subst (V "x", "y") (L "y" (V "y")) == L "y" (V "y")
-- case 2. the substitution is different from the bound variable, but the bound variable is not free in the substitution. Then we substitute in the body
test5 = subst (L "a" (V "a") , "x") (A (L "y" (V "x")) (V "y")) == A (L "y" (L "a" (V "a"))) (V "y")
-- case 3. the substitution is different from the bound variable, and the bound variable is free in the substitution. Then we substitute and rename.
test6 = subst (A (L "a" (V "y")) (V "a"), "x") (A (L "y" (V "x")) (V "y")) == A (L "b" (A (L "a" (V "y")) (V "a"))) (V "y")