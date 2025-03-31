-- Връща списък от всички елементи без последния
init' :: [a] -> [a]
init' [] = error "..."
init' (_:[]) = []
init' (x:xs) = x : init' xs

-- >>> init' [1, 2, 3, 4, 5]
-- [1,2,3,4]

-- >>> init' ["asd", "aaa", ['1', '2', '3'], "asd"]
-- ["asd","aaa","123"]

-- Връща последния елемент
last' :: [t] -> t
last' [] = error "..."
last' (x:[]) = x
last' (x:xs) = last' xs

-- >>> last' [1, 2, 3, 4, 5]
-- 5

-- Функция, която взима първите n елемента от списък или всички, ако са по-малко от n
take' _ [] = []
take' 0 _ = []
take' n (x:xs) = x : take' (n - 1) xs

-- >>> take' 10 [2, 5, 7, 1, 6, 3]
-- [2,5,7,1,6,3]

-- Ф-я, която премахва първите n елемента или всички, ако са по-малко
drop' _ [] = []
drop' 0 l = l
drop' n (_:xs) = drop' (n - 1) xs

-- >>> drop' 50 [2, 5, 7, 1, 6, 3]
-- []

-- splitAt n l - приема число и списък и връща tuple - наредена двойка от първите n и оставащите елементи
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n l = (take n l, drop n l)

-- maximum l - връща максималния елемент от списъка
maximum' :: Ord t => [t] -> t
maximum' [] = error "Cannot get maximum"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

-- minimum l - връща min елемент от списъка
minimum' :: Ord t => [t] -> t
minimum' [] = error "Cannot get minimum"
minimum' [x] = x
minimum' (x:xs) = min x (minimum' xs)

-- sum l - връща сумата на елементите в списъка
sum' :: Num a => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

-- product l
product' :: Num a => [a] -> a
product' [] = 1
product' (x:xs) = x * product' xs

-- and l - връща дали всички са истина (прилага логическо 'и')
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

-- or l - връща дали поне един е истина (прилага логическо 'или')
or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = x || or' xs

-- Ф-я, която приема списък от списъци и конкатенира списъците в един общ списък 
-- пр: [[1, 2, 3], [4, 5], [6, 7]] -> [1, 2, 3, 4, 5, 6, 7]
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

-- push - добавя елемнт в края на списък
push :: t -> [t] -> [t] 
push y [] = [y]
push y (x:xs) = (x:push y xs)

-- insert - приема елемент, индекст и списък и добавя елемента на този индекс в списъка
insert' x _ [] = [x]
insert' x 0 l = x:l
insert' x n (h:t) = h : (insert' x (n-1) t)

-- >>> insert' 32 1 [1, 2, 3]
-- [1,32,2,3]

-- >>> show (1:2:3:4:5:[])
-- "[1,2,3,4,5]"

-- >>> :t (+)
-- (+) :: Num a => a -> a -> a

-- Задача: Алгоритъма Merge Sort
merge :: Ord a => [a] -> [a] -> [a]
merge [] b = b
merge a [] = a
merge a@(x:xs) b@(y:ys)
    | x <= y = x : merge xs b  
    | otherwise = y: merge a ys

mergeSort [] = []
mergeSort [x] = [x]
mergeSort l = merge (mergeSort left) (mergeSort right)
    where (left, right) = splitAt (length l `div` 2) l

-- >>> mergeSort [2, 6, 1, 7, 16, -2]
-- [-2,1,2,6,7,16]

-- Ламбда функции и функции от по-висок ред
--id x = x
id = \x -> x

compose :: (t2 -> t1) -> (t3 -> t2) -> t3 -> t1
compose f g x = f (g x)

-- derivative f dx x

repeated _ 0 x = x
repeated f n x = f (repeated f (n-1) x)

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : (map' f xs) 

square x = x ^ 2

-- >>> map' square [1, 2, 3, 4, 5, 6]
-- [1,4,9,16,25,36]

filter' _ [] = []
filter' p (x:xs) = if p x then x : rest else rest
    where rest = filter' p xs

-- >>> filter' odd [1, 2, 3, 4, 5]
-- [1,3,5]

foldr' _ nv [] = nv
foldr' op nv (x:xs) = x `op` (foldr' op nv xs)

sum'' = foldr' (+) 0

-- >>> sum'' [1, 2, 3, 4]
-- 10

product'' = foldr' (*) 1

and = foldr' (&&) True
or = foldr' (||) False