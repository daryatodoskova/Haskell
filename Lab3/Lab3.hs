import Data.List


--- Zipping exercises


-- Exercise 1a
my_zip :: [a] -> [b] -> [(a,b)]
my_zip xs ys = zipWith (\x y -> (x,y)) xs ys

-- Exercise 1b
my_zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
my_zipWith f xs = map (uncurry f) . zip xs

-- Exercise 1c (optional)
my_transpose :: [[a]] -> [[a]]
my_transpose = undefined

--- Folding exercises

-- Exercise 2a
altsum :: Num a => [a] -> a
altsum = foldr (-) 0


-- Exercise 2b
my_intersperse :: a -> [a] -> [a]
my_intersperse v = foldr (\x xs -> if not (null xs) then x : v : xs else x : xs) []

-- Exercise 2c
my_tails :: [a] -> [[a]]
my_tails = foldr (\x xs -> (x : head xs) : xs) [[]]

-- Exercise 2d (optional)


-- Exercise 2e (optional)
my_dropWhile :: (a -> Bool) -> [a] -> [a]
my_dropWhile = undefined

--- Difference lists

type DiffList a = [a] -> [a]

toDL :: [a] -> DiffList a
toDL xs = (xs++)

fromDL :: DiffList a -> [a]
fromDL dxs = dxs []

cons :: a -> DiffList a -> DiffList a
cons x dxs = (x:) . dxs

snoc :: DiffList a -> a -> DiffList a
snoc dxs x = dxs . (x:)

-- Exercise 3a
toDLrev :: [a] -> DiffList a
toDLrev []     = toDL []
toDLrev (x:xs) = toDLrev xs . toDL [x]

-- Exercise 3b
my_reverse :: [a] -> [a]
my_reverse = fromDL . toDLrev

naive_reverse :: [a] -> [a]
naive_reverse []     = []
naive_reverse (x:xs) = naive_reverse xs ++ [x]

--- Regular expression matching

data RegExp = Zero | One
            | C Char
            | Plus RegExp RegExp | Times RegExp RegExp
            | Star RegExp
  deriving (Show,Eq)

accept :: RegExp -> String -> Bool

accept e w = acc e w null

-- Exercise 4a
acc :: RegExp -> String -> (String -> Bool) -> Bool
acc Zero          w k = False
acc One           w k = k w
acc (C a)         (w:ws) k = a==w && k ws
acc (Plus e1 e2)  w k = acc e1 w k || acc e2 w k
acc (Times e1 e2) w k = acc e1 w (\w' -> acc e2 w' k)

-- Exercise 4b (optional)
acc (Star e)      w k = undefined
