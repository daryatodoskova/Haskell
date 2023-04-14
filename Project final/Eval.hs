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



-- The function stepbeta performs a single beta-reduction on the input expression, trying the leftmost outermost beta-reductions first.
stepbeta :: LExp -> [LExp]  
stepbeta t = [(plug k (subst (t2,x) t1)) | (k, A (L x t1) t2) <- subexp t]
  where
    subexp :: LExp -> [LExpPtr]
    subexp t = go Hole t
      where
        go :: LExpCxt -> LExp -> [LExpPtr]
        go k t = (k,t) :
                 case t of
                   V x     -> []
                   A t1 t2 -> go (A'1 k t2) t1 ++ go (A'2 t1 k) t2
                   L x t1  -> go (L' x k) t1


-- stepbeta (A (L "x" (L "y" (A (V "y") (V "x")))) (L "z" (V "z")))

normalize :: LExp -> LExp 
normalize t = until isNormal (head . stepbeta) t
  where
    isNormal :: LExp -> Bool
    isNormal t = null (stepbeta t)

-- normalize (A (L "x" (L "y" (A (V "y") (V "x")))) (L "z" (V "z"))) expect L "z" (V "z")






