-- Задача 1: Да се напише функция, която приема предикат и списък от числа. Функцията трябва да разбива списъка на подсписъци, като използва за разделител елементите, за които предикатът връща True.

-- Имплементация на функцията break в Haskell
-- Погледнете си функциите break и span
splitByCondition :: (Integer -> Bool) -> [Integer] -> ([Integer], [Integer])
splitByCondition _ [] = ([], [])
splitByCondition p l@(x:xs)
    | p x = ([], l)
    | otherwise = (x:before, after)
    where (before, after) = splitByCondition p xs 

-- >>> splitByCondition (\x -> x `mod` 4 == 0) [1..13]
-- ([1,2,3],[4,5,6,7,8,9,10,11,12,13])

separateByCondition :: (Integer -> Bool) -> [Integer] -> [[Integer]]
separateByCondition _ [] = []
separateByCondition p l = 
    let (before, after) = splitByCondition p l   
    in before : case after of
                [] -> []
                (_:xs) -> separateByCondition p xs

-- >>> take 10 (separateByCondition (\x -> x `mod` 4 == 0) [1..])
-- [[1,2,3],[5,6,7],[9,10,11],[13,14,15],[17,18,19],[21,22,23],[25,26,27],[29,30,31],[33,34,35],[37,38,39]]



-- Задача 2 - да се напише функция, която проверява дали дадено число е "Мега Просто". Мега просто наричаме число, което е просто и на всички прости позиции в записа му (броейки отзад-напред) има прости цифри

isPrime :: Integer -> Bool
isPrime x = x > 1 && helper 2
        where helper i
                | i * i > x = True
                | x `mod` i == 0 = False
                | otherwise = helper (i + 1)

isPrimeV2 :: Integer -> Bool
isPrimeV2 x = x > 1 && all (\d -> x `mod` d /= 0) [2..floor (sqrt(fromIntegral x))]
-- >>> isPrimeV2 11
-- True

checkDigitsCondition :: Integer -> Bool
checkDigitsCondition n =  helper (n `div` 10) (n `mod` 10) 1
    where helper rem digit pos
            | rem == 0 = checkPosition
            | otherwise = checkPosition && nextStep
            where checkPosition = if isPrime pos then isPrime digit else True 
                  nextStep = helper (rem `div` 10) (rem `mod` 10) (pos + 1)

isMegaPrime :: Integer -> Bool
isMegaPrime x = isPrime x && checkDigitsCondition x



-- Вариант 2 - Използвайки Show и символи
isPrimeDigit :: Char -> Bool
isPrimeDigit c = c `elem` ['2', '3', '5', '7']

isMegaPrimeV2 :: Integer -> Bool
isMegaPrimeV2 n = isPrime n && all checkPrimePosition (zip [1..] (reverse (show n)))
  where checkPrimePosition (pos, digit) = if isPrime pos then isPrimeDigit digit else True



-- Задача 2.1 - да се напише функция, която генерира всички мега прости числа в интервала [a..b]

generateFromTo :: Integer -> Integer -> [Integer]
generateFromTo a b
    | a > b = []
    | otherwise = if isMegaPrime a then a : rest
                  else rest
    where rest = generateFromTo (a + 1) b

generateFromToV2 :: Integer -> Integer -> [Integer]
generateFromToV2 a b = [x | x <- [a..b], isMegaPrime x]
generateFromToV3 :: Integer -> Integer -> [Integer]
generateFromToV3 a b = filter isMegaPrime [a..b]

-- Да се генерират всички мега прости числа 

generateAll :: [Integer]
generateAll = [x | x <- [1..], isMegaPrime x]

generateAllV2 :: [Integer]
generateAllV2 = filter isMegaPrime [1..]

generateAllV3 :: [Integer]
generateAllV3 = generate 1
    where generate i = if isMegaPrime i then i : generate (i + 1)
                       else generate (i + 1)



-- Задача 3 - Вижте прикаченото изображение в папката

type Pilot = (String, Int, Int, Int)
type Round = (String, [Pilot])
type Season = [Round]

-- A)
-- Обърнете внимание на raceWinner дефиницията за намиране на пилота с най-много точки. Доста полезен израз при работа с кортежи, ако искате да работите без функции от по-висок ред (изключае map-а)

raceWinner :: [Pilot] -> String
raceWinner [] = "No winner" 
raceWinner ((name, pts, _, _):ps)
    | pts > (if null ps then 0 else maximum ptsMap) = name
    | otherwise = raceWinner ps
    where ptsMap = map (\(_,currPts,_,_) -> currPts) ps

removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates [] = []
removeDuplicates (x:xs)
    | x `elem` xs = removeDuplicates xs
    | otherwise = x: removeDuplicates xs

remDup :: Eq a => [a] -> [a]
remDup (x:xs) = x : filter (\v -> v /= x) (remDup xs)

allWinners :: Season -> [String]
allWinners races = remDup (map (\(_, pilots) -> raceWinner pilots) races)

-- B)

getPenalty :: Pilot -> Int
getPenalty (_,_,penalty,_) = penalty

getName :: Pilot -> String
getName (name, _, _, _) = name

partitionByName :: String -> [(String, Int)] -> ([(String, Int)], [(String, Int)])
partitionByName _ [] = ([],[])
partitionByName name (p:ps) =
    let (same, rest) = partitionByName name ps
    in if fst p == name then (p:same, rest) else (same, p:rest)

combineByName :: [(String, Int)] -> [(String, Int)]
combineByName [] = []
combineByName (p:ps) = 
    let (same, rest) = partitionByName (fst p) ps
    in (fst p, sum (map snd (p:same))) : combineByName rest

sortDesc :: [(String, Int)] -> [(String, Int)]
sortDesc [] = []
sortDesc (x:xs) =
    let larger = sortDesc [y|y <- xs, snd y >= snd x]
        smaller = sortDesc [y|y <- xs, snd y < snd x]
    in larger ++ [x] ++ smaller

penaltyImpact :: Season -> [(String, Int)]
penaltyImpact races = sortDesc(combineByName penalityList)
    where pilotsWithPenalty = [p| (_, pilots) <- races, p <- pilots, getPenalty p > 0]
          penalityList = [(getName p, getPenalty p)|p <- pilotsWithPenalty ]

-- C) 
-- Обърнете внимание на fastest дефиницията за намиране на най-бързия пилот. Доста полезен израз при работа с кортежи, ако разбирате функциите от по-висок ред. Сравнете го с функцията от точка А), която търси този с най-много точки

getLapTime :: Pilot -> Int
getLapTime (_,_,_,lap) = lap

fastestLap :: Season -> String
fastestLap [] = error "No such racer"
fastestLap races = getName (fastest allPilots)
    where allPilots = concatMap (\(_,pilots) -> pilots) races
          fastest (p:ps) = foldr (\curr acc -> if getLapTime curr < getLapTime acc then curr else acc) p ps

-- D)

missedFastestLapWins :: Season -> [String]
missedFastestLapWins [] = []
missedFastestLapWins ((trackName, pilots):rs) = 
    let fastest = getName (foldr1 (\curr acc -> if getLapTime curr < getLapTime acc then curr else acc) pilots)
        winner = raceWinner pilots
    in if fastest /= winner then trackName : rest else rest
    where rest = missedFastestLapWins rs



-- Задача 4 - Вижте прикаченото изображение в папката
windows :: Int -> [a] -> [[a]]
windows n xs
  | length chunk < n = []
  | otherwise = chunk : windows n (tail xs)
  where chunk = take n xs

movingAverage :: [Double] -> Int -> [Double]
movingAverage xs n = map avg (windows n xs)
  where avg ys = sum ys / fromIntegral n

allAverages :: [Double] -> [[Double]]
allAverages xs = map (\n -> movingAverage xs n) [2..]