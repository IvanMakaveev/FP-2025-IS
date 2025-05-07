myFilter p = foldr (mappingFn) []
    where mappingFn el acc = if p el then (el:acc) else acc

-- >>> myFilter odd [1, 2, 3, 4, 5]
-- [1,3,5]

myMap f l = foldr (\el acc -> f el : acc) [] l

-- >>> myMap (^2) [1, 2, 3, 4, 5, 6]
-- [1,4,9,16,25,36]

myFoldl _ nv [] = nv
myFoldl op nv (x:xs) = myFoldl op (nv `op` x) xs

-- flip f x y = f y x 

reverse3 l = foldl (flip (:)) [] l

-- >>> reverse3 [1, 2, 3, 4, 5]
-- [5,4,3,2,1]

zipWith' _ [] _ = [] 
zipWith' _ _ [] = []
zipWith' op (x:xs) (y:ys) = x `op` y : zipWith' op xs ys 

zip' l1 l2 = zipWith' (,) l1 l2  

-- >>> zipWith' (+) [1, 2, 3] [4, 5]
-- [5,7]

unzip l = foldr (\(a, b) (acc1, acc2) -> (a:acc1, b:acc2)) ([], []) l

-- За домашно пробвайте с образци и случаи
takeWhile' p l = foldr (\el acc -> if p el then el:acc else []) [] l

-- >>> takeWhile' odd [3, 7, 5, 9, 2, 11, 4]
-- [3,7,5,9]

-- dropWhile
dropWhile' p l = foldr (\el acc -> if p el then [] else el:acc) [] l

-- any 
any' p l = or (map p l)

-- >>> any' odd [2, 4, 6, 11, 12]
-- True

-- all
all' p l = and (map p l)

-- >>> all' even [2, 4, 6, 10, 12]
-- True

-- Задача: приемаме матрица от числа и списък от функции. Връщаме максимума от минимума по редове след прилагане на функциите върху всеки елемент от реда

rowMin row fnList = minimum (concatMap (\fn -> map fn row) fnList)
maxMin matrix fnList = maximum (map (\row -> rowMin row fnList) matrix)
-- maxMin matrix fnList = maximum (map (`rowMin` fnList) matrix)

-- >>> rowMin [1, 2, 3, 4] [(\x -> x + 1), (\x -> x*2)]
-- 2

ones = 1 : ones

-- >>> take 5 ones
-- [1,1,1,1,1]

nats = [0..]
-- enumFrom from
evens = [0, 2..]
-- enumFromThen from then

repeat' x = x : repeat' x 

-- >>> take 50 (repeat' 10)
-- [10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10]

cycle' l = l ++ cycle' l

-- >>> take 50 (cycle' [1, 2, 3])
-- [1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2]

iterate' f z = z : iterate' f (f z)

-- >>> take 50 (iterate' (+10) 2)
-- [2,12,22,32,42,52,62,72,82,92,102,112,122,132,142,152,162,172,182,192,202,212,222,232,242,252,262,272,282,292,302,312,322,332,342,352,362,372,382,392,402,412,422,432,442,452,462,472,482,492]

-- >>> take 50 [1..]
-- [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50]

-- >>> take 50 $ filter even [1..]
-- [2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66,68,70,72,74,76,78,80,82,84,86,88,90,92,94,96,98,100]



-- Сумата на квадратите на нечетните числа от 1 до 10
-- >>> sum $ [x^2|x<-[1..10], (\d -> d `mod` 2 == 1) x]
-- 165


-- >>> sum (map (^2) (filter odd [1..10]))
-- 165


myFunct :: (Integral a) => [a] -> a
myFunct = sum . map (^2) . filter odd

-- >>> myFunct [1..11]
-- 286