
-- Да се напише функция, която пресмята модул от X
myAbs :: Int -> Int
myAbs x = if x > 0 then x else -x

-- >>> myAbs -5
-- 5

-- Брои цифрите на някое число
countDigits :: Int -> Int
countDigits 0 = 1
countDigits n = helper n 0
    where helper 0 k = k
          helper x k = helper (x `div` 10) (k + 1)

-- >>> countDigits 123
-- 3

isPrefix :: Int -> Int -> Bool
isPrefix num prf
    | num == prf = True
    | num < prf = False
    | otherwise = isPrefix (num `div` 10) prf

-- >>> isPrefix 19741 18
-- False

lastDig y = y `mod` 10
remLast y = y `div` 10

reverseNum :: Int -> Int
reverseNum x = revHelper x 0
    where revHelper 0 acc = acc
          revHelper y acc = revHelper (remLast y) (acc * 10 + lastDig y)

-- >>> reverseNum 12345
-- 54321

isPalindrome :: Int -> Bool
isPalindrome x = x == reverseNum x

isPrime :: Int -> Bool
isPrime n = (n > 1) && iter 2 
    where iter i
            | i * i > n = True
            | n `mod` i == 0 = False
            | otherwise = iter (i + 1)

-- >>> isPrime 23
-- True

sumDivisors :: Int -> Int
sumDivisors n = iter 1
    where iter i
            | i == n = 0
            | otherwise = (if n `mod` i == 0 then i else 0) + iter (i + 1)

isPerfect :: Int -> Bool
isPerfect n = n == sumDivisors n

fastPower _ 0 = 1
fastPower x n
    | even n = sq half
    | otherwise = x * sq half
    where sq a = a * a
          half = fastPower x (n `div` 2)

sumInterval :: Int -> Int -> Int
sumInterval a b = if a > b then 0
                  else a + sumInterval (a + 1) b
            
accumulate op nv a b term next = if a > b then nv
            else op (term a) (accumulate op nv (next a) b term next)
