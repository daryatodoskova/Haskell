-- ex1

msort :: Ord a => [a] -> [a]
msort [] = []
msort [a] = [a]
msort as = merge(msort start) (msort end)
           where (start, end) = splithalf as

splithalf :: [a] -> ([a], [a])
splithalf xs = (take lhx xs, drop lhx xs)
           where lhx = length xs `div` 2

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) = if x <= y
                      then x : merge xs (y:ys)
                      else y : merge (x:xs) ys

-- ex2

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = if x <= y
                    then isSorted (y:xs)
                    else False

-- ex3

main :: IO ()
main = interact(unlines . msort . lines)
