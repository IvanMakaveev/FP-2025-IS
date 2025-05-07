dotProduct xs ys = sum $ zipWith (*) xs ys

-- For a given list [a1, a2... an] find [a1, a2 - a1, a3 - a2..., an - an-1]
difference [] = []
diffrerence xs = head xs : zipWith (-) (tail xs) xs
 
-- Function that takes ever n-th number
everyNth n l = [x|(x, i) <- zip l [0..], i `mod` n == 0]
 
-- Function that calculates tribonacci
tribonacci = 0 : 1 : 1 : zipWith3 (\a b c -> a+b+c) tribonacci (tail tribonacci) (drop 2 tribonacci)
 
-- Generate all valid parenthesis wit n pairs
generate' 0 = [""]
generate' k = [c:rest| c <- "()", rest <- generate' (k-1)]
 
validParentesis n = [p | p <- generate(2*n), isValid p]
    where generate 0 = [""]
          generate k = [c:rest| c <- "()", rest <- generate (k-1)]
          isValid expr = check 0 expr
          check a [] = a == 0
          check a (x:xs)
            | x == '(' = check (a+1) xs
            | x == ')' = a > 0 && check (a-1) xs
            | otherwise = False
 
-- Generate all knight moves from a point 
-- TODO
 
-- Generate all goldbach pairs for a number n
--goldbachPairs n = [(p, (n-p))|p<-[2..n `div` 2], isPrime p, isPrime (n-p)]
 
-- Bonus - generate all goldbach pairs in order for all numbers
--allGoldbach = concatMap goldbachPairs [1..]
 
-- Find fixed point after iterations (when does f(f(....(f(x)))) stop changing)
-- TODO
 
-- Find all rows of a matrix with larger even sum than odd sum 
-- TODO

--- Чрез foldr проверка дали е сортиран
isSorted l = snd $ foldr (\el (prev, isSorted) -> if el <= prev then (el, isSorted) else (el, False)) (last l, True) l
 
data DayOfWeek = Mon | Tue | Wed
data Bool = False | True
 
data Shape = Circle {radius::Int} | Rectangle {width::Int, height::Int} deriving Eq
 
data Maybe a = Just a | Nothing
 
data Tree a = Empty | Node a (Tree a) (Tree a)