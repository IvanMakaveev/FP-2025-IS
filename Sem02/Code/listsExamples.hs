-- (:) :: a -> [a] -> [a]   - Конструктор на списък с елементи от тип а
-- >>> 1:2:[]                
-- [1,2]

head' :: [a] -> a
head' [] = error "Cannot get the head of empty list"
head' (h:_) = h

-- >>> head' [1, 2, 3, 4]
-- 1
-- >>> head' "asdasdasd"
-- 'a'

tail' :: [a] -> [a]
tail' [] = error "Cannot get the tail of empty list"
tail' (_:t) = t

null' :: [a] -> Bool
null' [] = True
null' _ = False

length' :: [a] -> Int
length' [] = 0
length' (_:t) = 1 + length' t

-- >>> length' [1, 2, 3, 5]
-- 4

enumFromTo' :: Int -> Int -> [Int]
enumFromTo' a b = if a > b then []
                  else a : enumFromTo' (a + 1) b


(+++) :: [a] -> [a] -> [a]
[] +++ l = l
(h:t) +++ l = h : (t +++ l)   

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = (reverse' xs) +++ [x]

(!!!) :: [a] -> Int -> a
[] !!! _ = error "Index too large"
(x:_) !!! 0 = x
(_:xs) !!! i = xs !!! (i - 1)  

-- >>> [3, 4, 7] !!! 1
-- 4

elem' :: Eq t => [t] -> t -> Bool
elem' [] _ = False
elem' (x:xs) el
    | x == el = True
    | otherwise = elem' xs el
