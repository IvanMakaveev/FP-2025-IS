nats' = [1..]

nats'' = 1 : map (+1) nats''


pythagoreanTriples = [(x, y, z)| z<-[1..], y<-[1..z], x<-[1..y], (x^2 + y^2) == (z^2)]

-- >>> take 10 pythagoreanTriples
-- [(3,4,5),(6,8,10),(5,12,13),(9,12,15),(8,15,17),(12,16,20),(15,20,25),(7,24,25),(10,24,26),(20,21,29)]

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- >>> take 50 fibs
-- [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,6765,10946,17711,28657,46368,75025,121393,196418,317811,514229,832040,1346269,2178309,3524578,5702887,9227465,14930352,24157817,39088169,63245986,102334155,165580141,267914296,433494437,701408733,1134903170,1836311903,2971215073,4807526976,7778742049]


primes = sieve [2..]
    where sieve (x:xs) = x : sieve (filter (\n -> n `mod` x /= 0) xs)

-- >>> take 50 primes
-- [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229]

points = [(y, x - y)| x <- [0..], y<-[0..x]]

-- >>> take 50 points
-- [(0,0),(0,1),(1,0),(0,2),(1,1),(2,0),(0,3),(1,2),(2,1),(3,0),(0,4),(1,3),(2,2),(3,1),(4,0),(0,5),(1,4),(2,3),(3,2),(4,1),(5,0),(0,6),(1,5),(2,4),(3,3),(4,2),(5,1),(6,0),(0,7),(1,6),(2,5),(3,4),(4,3),(5,2),(6,1),(7,0),(0,8),(1,7),(2,6),(3,5),(4,4),(5,3),(6,2),(7,1),(8,0),(0,9),(1,8),(2,7),(3,6),(4,5)]

-- >>> pack [2, 2, 2, 4, 2, 3, 4, 5, 6, 7]
-- [[2,2,2],[4],[2],[3],[4],[5],[6],[7]]

-- >>> span (==2) [2, 2, 2, 4, 2, 3, 4, 5, 6, 7]
-- ([2,2,2],[4,2,3,4,5,6,7])

pack [] = []
pack l@(x:xs) = first:pack second
    where(first, second) = break (/= x) l


slidingWindow n xs
    | length chunck < n = []
    | otherwise = chunck : (slidingWindow n $ tail xs)
    where chunck = take n xs 

movingAverages l n = map avg windows
    where windows = slidingWindow n l
          avg xs = sum xs / (fromIntegral n)


-- >>> take 50 (movingAverages (map fromIntegral fibs) 7)
-- [2.857142857142857,4.714285714285714,7.571428571428571,12.285714285714286,19.857142857142858,32.142857142857146,52.0,84.14285714285714,136.14285714285714,220.28571428571428,356.42857142857144,576.7142857142857,933.1428571428571,1509.857142857143,2443.0,3952.8571428571427,6395.857142857143,10348.714285714286,16744.571428571428,27093.285714285714,43837.857142857145,70931.14285714286,114769.0,185700.14285714287,300469.14285714284,486169.28571428574,786638.4285714285,1272807.7142857143,2059446.142857143,3332253.8571428573,5391700.0,8723953.857142856,1.4115653857142856e7,2.2839607714285713e7,3.6955261571428575e7,5.979486928571428e7,9.675013085714285e7,1.5654500014285713e8,2.53295131e8,4.0984013114285713e8,6.631352621428572e8,1.0729753932857143e9,1.7361106554285715e9,2.809086048714286e9,4.545196704142858e9,7.354282752857142e9,1.1899479457e10,1.9253762209857143e10,3.1153241666857143e10,5.040700387671429e10]

type UnaryOperator a = a -> a
type Matrix a = [[a]]
type Dictionary k v = [(k, v)]

-- >>> :k Dictionary
-- Dictionary :: * -> * -> *

f10 :: UnaryOperator Int
f10 x = x + 1

last'' :: [t] -> t
last'' [] = error "..."
last'' (x:[]) = x
last'' (x:xs) = last' xs

myList :: [t]
myList = []

-- class Eq a where
--     (==), (/=) :: a -> a -> Bool
--     x /= y = not (x == y)
--     x == y = not (x /= y)

class Measurable a where
    size :: a -> Integer

-- >>> :k Measurable
-- Measurable :: * -> Constraint

elem'' :: Eq t => [t] -> t -> Bool
elem'' [] _ = False
elem'' (x:xs) el
    | x == el = True
    | otherwise = elem'' xs el

-- >>> elem'' [] (\x -> x + 1)

larger :: Measurable a => a -> a -> Bool
larger x y = size x > size y

instance Measurable Integer where
    size :: Integer -> Integer
    size 0 = 0
    size x = 1 + size (x `div` 10)

    
instance Measurable Double where
    size :: Double -> Integer
    size x = size (toInteger $ floor x)

class Derivable a where
    derive :: a -> a


instance (Measurable a, Measurable b) => Measurable (a, b) where
    size (a, b) = size a + size b


-- >>> larger (1, 1) (2, 2)
-- False

-- >>> larger (1.5, 1.5) (2.5, 2.5)
-- False

-- >>> larger (1, 17.5) (2.5, 2)
-- True