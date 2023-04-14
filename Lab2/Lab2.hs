--- Proving exercises (from Lecture 1 notes)

-- Exercise 3.1
{-
  Let's prove that lists form a monoid under concatenation
  with nil as the identity element, by proving the following equations
  [] ++ xs = xs (1)
  xs ++ [] = xs (2)
  xs ++ (ys ++ zs) = (xs ++ ys) ++ zs (3)

  For (1)
  It is trivial that an empty list concatenated with any list is equal to the said list.
  Thus [] ++ xs = xs is True.

  For (2) by structural induction
  Assume P(xs) := xs ++ [] = xs is true for any list xs.
  Then, P(x:xs) = (x:xs) ++ [] = x:(xs ++ []).
  By assumption, we have P(x:xs) = (x:xs) ++ [] = x ++ xs = x:xs.
  Thus, P(xs) => P(x:xs) and xs ++ [] = xs is true for any xs.

  For (3) by structural induction
  Assume P(xs) := xs ++ (ys ++ zs) = (xs ++ ys) ++ zs is true for any lists xs, ys and zs.
  Then, P(x:xs) = (x:xs) ++ (ys ++ zs) = x:((xs ++ (ys ++ zs)))
  By assumption, we get P(x:xs) = (x:xs) ++ (ys ++ zs) = x:((xs ++ ys) ++ zs) = (x:xs ++ ys) ++ zs
  Thus, P(xs) => P(x:xs) and (x:xs) ++ (ys ++ zs) = (x:xs ++ ys) ++ zs for any xs, ys and zs.

-}

-- Exercise 3.2 (optional)

-- Exercise 3.3 (optional)

-- Exercise 3.4 (optional)

import Data.List
import Data.Maybe

--- Programming exercises

-- Exercise 0a
doubleList :: [a] -> [a]
doubleList [] = []
doubleList (x : xs) = [x, x] ++ doubleList xs

-- Exercise 0b
firstDoubled :: Eq a => [a] -> Maybe a
firstDoubled [] = Nothing
firstDoubled [a] = Nothing
firstDoubled (x:y:xs) = if x == y then Just x else firstDoubled (y:xs)

data Allergen = Nuts | Gluten | Soy | Dairy      deriving (Show, Eq)

type Recipe   = [Allergen]

type Name     = String
type Price    = Int
data Cupcake  = CC Name Recipe Price             deriving (Show,Eq)

r1, r2, r3, r4, r5 :: Recipe
r1 = [Gluten]
r2 = []
r3 = [Nuts]
r4 = [Dairy,Gluten]
r5 = [Soy]

onsale :: [Cupcake]
onsale = [CC "Chocolate Surprise" r1 200,
          CC "Lemon Mayhem" r2 150,
          CC "Peanut Butter Bliss" r3 150,
          CC "Yogurt Truly" r4 250,
          CC "Caramel Karma" r5 200]

-- Exercise 1a
priceInRange :: Price -> Price -> Cupcake -> Bool
priceInRange min max (CC n r p) = if p >= min && p <= max then True else False

priceRange :: Price -> Price -> [Cupcake] -> [Name]
priceRange _ _ [] = []
priceRange min max ((CC n r pr) : cs) = if priceInRange min max (CC n r pr) then [n] ++ priceRange min max cs else priceRange min max cs

-- Exercise 1b
isInList :: [Allergen] -> Allergen -> Bool
isInList as a = if elem a as then True else False

hasAllergens :: [Allergen] -> Cupcake -> Bool
hasAllergens _ (CC _ [] _) = False
hasAllergens allergens (CC n (x:xs) p) = if isInList allergens x then True else hasAllergens allergens (CC n xs p)


allergyFree :: [Allergen] -> [Cupcake] -> [Name]
allergyFree _ [] = []
allergyFree as ((CC n r pr) : cs) =
  if hasAllergens as (CC n r pr) then allergyFree as cs else [n] ++ allergyFree as cs


type Tin = [Recipe]
data Spec = And Spec Spec | Or Spec Spec | Not Spec | HasCup Int Allergen  deriving (Show,Eq)

sampletin :: Tin
sampletin = [r3,r4,r2,r5]

-- Exercise 2a
checkSpec :: Spec -> Tin -> Bool
checkSpec (And s1 s2) tin = (checkSpec s1 tin) && (checkSpec s2 tin)
checkSpec (Or s1 s2) tin = (checkSpec s1 tin) || (checkSpec s2 tin)
checkSpec (Not s) tin = not (checkSpec s tin)
checkSpec (HasCup k x) tin = elem x (tin !! k)

-- Exercise 2b (optional)
checkSpec' :: Spec -> Tin -> Maybe Bool
checkSpec' (HasCup k x) tin = if k >= 0 && k < (length tin) then Just (checkSpec (HasCup k x) tin) else Nothing

data Tree a b = Leaf a | Node b [Tree a b]  deriving (Show,Eq)

texample :: Tree Char Integer
texample = Node 1 [Node 2 [Leaf 'a', Leaf 'b'], Node 3 [Leaf 'c', Leaf 'd', Leaf 'e'], Node 4 []]

bst :: Tree () Char
bst = Node 'c' [Node 'a' [Leaf (), Node 'b' [Leaf (), Leaf ()]], Node 'd' [Leaf (), Leaf ()]]

-- Exercise 3a
forest_canopy :: [Tree a b] -> [a]
forest_canopy [] = []
forest_canopy [t] = canopy t
forest_canopy (t : trees) = canopy t ++ forest_canopy trees

canopy :: Tree a b -> [a]
canopy (Leaf a) = [a]
canopy (Node a tree) = forest_canopy tree

-- Exercise 3b (optional)

forest_preorder :: [Tree a b] -> [Either a b]
forest_preorder [] = []
forest_preorder [tree] = preorder tree
forest_preorder (tree : trees) = preorder tree ++ forest_preorder trees

preorder :: Tree a b -> [Either a b]
preorder (Leaf a) = [Left a]
preorder (Node a tree) = Right a : forest_preorder tree

-- Exercise 4
linearSortAux :: Ord a => [a] -> [a] -> [a]
linearSortAux [] x = x
linearSortAux (x : xs) [] = linearSortAux xs [x]
linearSortAux (x : xs) (s : ss) =
  if x > s
    then s : linearSortAux (x : xs) ss
    else linearSortAux xs (x : s : ss)

linearSort :: Ord a => [a] -> [a]
linearSort a = linearSortAux a []

-- Exercise 5a (optional)
counterexample :: [Int]
counterexample = subsequences [1..9]

data Bin = L | B Bin Bin  deriving (Show,Eq)

-- Exercise 5b (optional)
fromBin :: Bin -> [Int]
fromBin B n bin = permutation
toBin :: [Int] -> Maybe Bin
toBin xs = if linearSort xs == [1..n] then Just B Bin Bin else Nothing
