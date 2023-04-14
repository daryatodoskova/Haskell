--- Programming in untyped lambda calculus

-- Encodings of booleans and natural numbers from class
{-
true = \x.\y.x
false = \x.\y.y
not = \b.\x.\y.b y x
and = \b.\c.b c false
zero = \f.\x.x
one = \f.\x.f x
two = \f.\x.f (f x)
succ = \n.\f.\x.f (n f x)
add = \m.\n.m succ n
mult = \m.\n.m (add n) 0
isZero = \n.n (\b.false) true
-}

-- Exercise 1a
{-
isEven = \n.n not true
-}

-- Exercise 1b
{-
exp = \mn.n (mult n) one
-}

-- Encodings of pairing and projections
{-
pair = \x.\y.\f.f x y
fst = \p.p (\x.\y.x)
snd = \p.p (\x.\y.y)
-}

-- Exercise 1c
{-
swap = \p. pair (snd p) (first p)
-}

-- Exercise 1d
{-
swapIf = \t.\p.t swap p
-}

-- Exercise 1e (optional)
{-
fib_iter = \fib_iter'.
           \n.((isZero n)
                 zero
                 ((isZero (pred n))
                    one
                    (add
                       (fib_iter' (pred n))
                       (fib_iter' (pred (pred n))))))

fib = Y fib_iter
-}

-- Exercise 1e (optional)
-- using isZero function to handle the 0 case
{-
pred = \n.((isZero n) n (n false))
-}

-- Curry's and Turing's fixed point operators
{-
Y = \x.(\y.x(y y))(\y.x (y y))
Theta = (\x.\y.y (x x y)) (\x.\y.y (x x y))
-}

-- Exercise 1f (optional)
{-
collatz = <your definition here>
-}

--- STLC and type inference

-- Exercise 2a
{-
e1 :: (t0 -> t1) -> t0 -> t1
e2 :: (t -> t) -> t -> t
e3 :: ((p -> p) -> t) -> t
e4 :: (t1 -> t1 -> t2) -> t1 -> t2
e5 :: No general type (thus no simple type either)
e6 :: (t1 -> t2) -> ((t1 -> t2) -> t1) -> t2
-}


-- Exercise 2b
fn1 :: a -> b -> (a -> b -> c) -> c
fn1 a b f = f a b
fn2 :: a -> b -> (a -> b -> c) -> (a,b,c)
fn2 a b f = (a, b, (f a b))
fn3 :: ([a] -> b) -> a -> b
fn3 f a = f [a]
fn4 :: ((a -> a) -> b) -> b
fn4 = f fn5
fn 5 :: a -> a
fn5 a = a

-- Exercise 2c (optional)
{-
mysterylam = ??
-}


-- Exercise 2d (optional)
mysteryfn = undefined

--- Bidirectional typing

data Ty = TV Int | Fn Ty Ty
    deriving (Show,Eq)

data Expr = V Int | A Expr Expr | L Int Expr | Ann Expr Ty
    deriving (Show,Eq)

bcomp = L 0 $ L 1 $ L 2 $ A (V 0) (A (V 1) (V 2))

oneid = A (Ann (L 0 $ L 1 $ A (V 0) (V 1)) (Fn (Fn (TV 0) (TV 0)) (Fn (TV 0) (TV 0)))) (L 0 $ V 0)

type TyCxt = [(Int,Ty)]

check :: TyCxt -> Expr -> Ty -> Bool
synth :: TyCxt -> Expr -> Maybe Ty

-- Exercise 3
check s t S T = ??
synth e f = ??
