data WeekDay = Mon | Tue | Wed | Sat | Sun
    deriving (Eq, Ord)


isWeekend :: WeekDay -> Bool
isWeekend Sun = True 
isWeekend Sat = True 
isWeekend _ = False

data Player = Player String Int

getName :: Player -> String
getName (Player n _) = n

getPlayerAge :: Player -> Int
getPlayerAge (Player _ a) = a

myPlayer = Player "Pesho" 12

-- >>> getName myPlayer
-- "Pesho"

-- >>> getPlayerAge myPlayer
-- 12


generatePlayer :: String -> Player
generatePlayer name = Player name (length name)

-- >>> getPlayerAge $ generatePlayer "Pesho"
-- 5


data AdvancedPlayer = AdvancedPlayer {name :: String, age :: Integer, player :: Player}

-- >>> player (AdvancedPlayer "Gosho" 20 (Player "Pesho" 12))
-- No instance for `Show Player' arising from a use of `evalPrint'
-- In a stmt of an interactive GHCi command: evalPrint it_aoa3R


data Shape = Rectangle {width, height :: Double} | Circle {radius :: Double}
    deriving Eq

-- >>> width (Rectangle 6 5)
-- 6.0

area :: Shape -> Double
area (Rectangle w h) = w * h
area (Circle r) = r * r * 3.14

-- >>> area $ getShapesFromFile "myFile.txt"

--              [Shape]

-- instance Eq Shape where
--     (==) :: Shape -> Shape -> Bool
--     Circle x == Circle y = x == y
--     Rectangle a b == Rectangle c d = a == b && c == d
--     _ == _ = False
    
-- >>> Rectangle 5 5 == Circle 5
-- False

instance Show Shape where
    show (Circle a) = "Circle: " ++ show a 
    show (Rectangle a b) = "Rectangle: " ++ show a ++ " " ++ show b

-- >>> show (Circle 5) 
-- "Circle: 5.0"

-- data Maybe a = Nothing | Just a
--     deriving (Eq, Ord, Show, Read)


getAt :: [a] -> Integer -> Maybe a
getAt [] _ = Nothing
getAt (x:xs) 0 = Just x
getAt (x:xs) n = getAt xs (n - 1)

extract :: Maybe Int -> Int
extract (Just x) = x
extract Nothing = error "..."


-- >>> getAt [Circle 2, Rectangle 2 2] 1
-- Just Rectangle: 2.0 2.0

-- >>> :k Maybe Int
-- Maybe Int :: *


-- data MyEither a b = Left a | Right b

-- >>> :k MyEither Int
-- MyEither Int :: * -> *


data MyPlayer = MyPlayer {playername::String, score::Int}

-- Да се напише функция, която приема списък от играчи (всеки играч се състои от име и точки) и ни връща най-големият резултат, ако той е единствен, или списък от имената на играчите с най-голям резултат в противен случай
searchBest :: [MyPlayer] -> Either Int [String]
searchBest ps
    | length bestPlayers == 1 = Left bestScore
    | otherwise = Right $ map playername bestPlayers
        where bestScore = maximum $ map (\p -> score p) ps
              bestPlayers = filter (\p -> bestScore == score p) ps 

-- >>> searchBest []
-- Right []