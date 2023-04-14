import Control.Monad.State
import Control.Monad.Fail
import System.Random
import Data.List

data Expr = Con Double | Sub Expr Expr | Div Expr Expr
    deriving (Show,Eq)

e1 = Sub (Div (Con 2) (Con 4)) (Con 3)
e2 = Sub (Con 1) (Div (Con 2) (Con 2))
e3 = Div (Con 1) (Sub (Con 2) (Con 2))


-- Exercise 1a

evalSafe :: Expr -> Maybe Double
evalSafe (Con c)     = return c
evalSafe (Sub e1 e2) =
  do { x1 <- evalSafe e1
      ; x2 <- evalSafe e2
      ; return (x1 - x2)
  }
evalSafe (Div e1 e2) =
  do { x1 <- evalSafe e1
     ; x2 <- evalSafe e2
     ; if x2 /= 0 then return (x1 / x2) else Nothing
  }

-- Exercise 1b
evalSafeMF :: MonadFail m => Expr -> m Double
evalSafeMF (Con c)     = return c
evalSafeMF (Sub e1 e2) =
  do { x1 <- evalSafeMF e1
      ; x2 <- evalSafeMF e2
      ; return (x1 - x2)
  }
evalSafeMF (Div e1 e2) =
  do { x1 <- evalSafeMF e1
     ; x2 <- evalSafeMF e2
     ; if x2 /= 0 then return (x1 / x2) else fail "division by zero"
  }

{- different outputs of evalSafeMF:
The Maybe monad : Nothing.
The List monad : [].
The IO monad (by default) : *** Exception: user error (division by zero)
-}

--------------------------------------------

-- Exercise 1c
evalWeird' :: MonadFail m => Expr -> StateT Int m Double
evalWeird'(Con c) = do
  n <- get
  put (n+1)
  return (if n `mod` 3 == 2 then 0 else c)
evalWeird' (Sub e2 e1) = do
  x1 <- evalWeird' e1
  x2 <- evalWeird' e2
  return (x2-x1)
evalWeird' (Div e2 e1) = do
  x1 <- evalWeird' e1
  x2 <- evalWeird' e2
  if x1/=0 then return (x2/x1) else fail "division by zero"

evalWeirdTop' :: MonadFail m => Expr -> m Double
evalWeirdTop' e = do
  (x,s) <- runStateT (evalWeird' e) 0
  return x

data Bin a = L a | B (Bin a) (Bin a)
  deriving (Show,Eq)

mapBin :: (a -> b) -> Bin a -> Bin b
mapBin f (L x)     = L (f x)
mapBin f (B tL tR) = B (mapBin f tL) (mapBin f tR)

instance Functor Bin where
  fmap = mapBin

-- Exercise 2a
{-
For all binary trees t :: Bin a and functions f :: b -> c and g :: a -> b,

1) It is trivial that mapBin id t = t.

2) let's show that mapBin (f . g) t = mapBin f (mapBin g t).
We have fmap (f . g) t == (fmap f . fmap g) t.


-}

-- Exercise 2b
instance Monad Bin where
  return = undefined
  (>>=) = undefined

instance Applicative Bin where
  pure = return
  xm <*> ym = xm >>= \x -> ym >>= return . x

-- Exercise 2c (optional)
{- Your proof goes here -}

-- Exercise 2d (optional)
{- Your thoughts go here -}

class Monad m => SelectMonad m where
  select :: [a] -> m a

instance SelectMonad [] where
  select = id

instance SelectMonad IO where
  select xs
    | not (null xs) = do i <- getStdRandom (randomR (0, length xs-1))
                         return (xs !! i)
    | otherwise     = fail "cannot select from empty list"

newtype Dist a = Dist { dist :: [(a,Rational)] }  deriving (Show)

instance Monad Dist where
  return x = Dist [(x,1)]
  xm >>= f = Dist [(y,p*q) | (x,p) <- dist xm, (y,q) <- dist (f x)]

-- We add the following standard boilerplate to derive instances of the
-- Functor and Applicative type classes, from the Monad instance above:
instance Functor Dist where
  fmap f xm = xm >>= return . f

instance Applicative Dist where
  pure = return
  xm <*> ym = xm >>= \x -> ym >>= return . x

instance SelectMonad Dist where
  select xs
    | not (null xs) = let n = length xs in Dist [(x, 1 / fromIntegral n) | x <- xs]
    | otherwise     = error "cannot select from empty list"

code :: SelectMonad m => m Char
code = do
  i <- select [0..3]
  return ("hello" !! i)

prob :: Eq a => Dist a -> a -> Rational
prob xm x = sum [p | (y,p) <- dist xm, x == y]

normalize :: Eq a => Dist a -> Dist a
normalize xm = Dist [(x,prob xm x) | x <- support xm]
  where
    support :: Eq a => Dist a -> [a]
    support xm = nub [x | (x,p) <- dist xm, p > 0]  -- "nub", defined in Data.List, removes duplicates

-- Exercise 3a
coin :: SelectMonad m => m Bool
coin = select [True, False]

-- Exercise 3b
subset :: SelectMonad m => [a] -> m [a]
subset [] = []
subset xs = do
    i <- pick 0 (length xs - 1)
    return (xs !! i)


-- Exercise 3c
simulate :: Monad m => Int -> m Bool -> m Int
simulate 0 bm = return 0
simulate n bm = do
  k <- simulate (n -1) bm
  b <- bm
  if b then return (k + 1) else return k


-- Exercise 3d (optional)
genTree :: SelectMonad m => [a] -> m (Bin a)
genTree [x] = return (L x)
genTree (x:xs) = do
                 tx <- genTree xs
                 i <- pick 0 (2*(length xs) - 2)
                 (tx', n) <- replace i x tx
                 return (tx') where
                     replace :: SelectMonad m => Int -> a -> Bin a -> m ((Bin a), Int)
                     replace 0 y t = do
                         l <- choose [True, False]
                         if l == True then return (B (L y) t, 0)
                         else return (B t (L y), 0)
                     replace n y (L z) = return ((L z), (n))
                     replace n y (B ta tb) = do
                         (t, n') <- replace (n-1) y ta
                         if n' <= 0 then return ((B t tb), n')
                         else do
                             (t', n'') <- replace (n'-1) y tb
                             return ((B ta t'), n'')
