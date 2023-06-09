module Eval (normalize) where

import Expr
import Subst

-- datatype of one-hole contexts for lambda expressions
data LExpCxt = Hole | A'1 LExpCxt LExp | A'2 LExp LExpCxt | L' Var LExpCxt
  deriving (Show,Eq)

-- we represent contexts "inside-out", i.e., with the parts of the
-- context nearest to the hole at the top-level.
-- The plugging function is defined accordingly.
plug :: LExpCxt -> LExp -> LExp
plug Hole       d = d
plug (A'1 c e2) d = plug c (A d e2)
plug (A'2 e1 c) d = plug c (A e1 d)
plug (L' x c)   d = plug c (L x d)

-- a pointer to a subexpression (a.k.a. "zipper") is a pair of a context and an expression
type LExpPtr = (LExpCxt,LExp)

-- a template for implementing normalize, as described in the
-- mini-project page...


subexp :: LExp -> [LExpPtr]
subexp (V x) = [(Hole, V x)]
subexp (A e1 e2) = ((Hole, A e1 e2) : foldr (\(c, expr) res -> (A'1 c e2, expr) : res) [] (subexp e1)) 
                    ++ foldr (\(c, expr) res -> (A'2 e1 c, expr) : res) [] (subexp e2)
subexp (L x e) = (Hole, L x e) : foldr (\(c, expr) res -> (L' x c, expr) : res) [] (subexp e)


--Use subexp to write a function redex :: LExp -> [LExpPtr] that generates pointers to all beta-redices in a given expression, starting with the leftmost outermost redices.
redex :: LExp -> [LExpPtr]
redex e = filter (\(cxt, exp) -> case exp of
                                    (A (L x e1) e2) -> True
                                    otherwise -> False) (subexp e)



--Use redex to write a function stepBeta :: LExp -> [LExp] that non-deterministically performs a beta-reduction on the input expression, trying the leftmost outermost beta-reductions first.
stepBeta :: LExp -> [LExp]
stepBeta e = map (\(cxt, exp) -> case exp of
                                    (A (L x e1) e2) -> plug cxt (subst (e2, x) e1)
                                    otherwise -> e) (redex e)



--Use stepBeta to write normalize by repeatedly performing leftmost outermost beta-reductions until you reach a normal form (or the universe collapses).
normalize :: LExp -> LExp
normalize e = case stepBeta e of
                [] -> e
                otherwise -> normalize (head (stepBeta e))


test :: LExp -> [LExp]
test e = e : test (normalize e)