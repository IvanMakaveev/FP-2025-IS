data Nat = Zero | Succ Nat

four = Succ $ Succ $ Succ $ Succ $ Zero

fromNatToInt :: Nat -> Int
fromNatToInt Zero = 0
fromNatToInt (Succ n) = 1 + fromNatToInt n

-- >>> fromNatToInt four
-- 4

data Bin = One | BitZero Bin | BitOne Bin

binarySix :: Bin
binarySix = BitZero $ BitOne $ One

fromBinaryToDecimal :: Bin -> Int
fromBinaryToDecimal One = 1
fromBinaryToDecimal (BitZero rest) = 2 * fromBinaryToDecimal rest
fromBinaryToDecimal (BitOne rest) = 2 * fromBinaryToDecimal rest + 1

-- >>> fromBinaryToDecimal binarySix
-- 6


data List a = Nil | Element a (List a)


data Tree a = Node a (Tree a) (Tree a) | Empty deriving Show

getValue :: Tree a -> a
getValue Empty = error "No value"
getValue (Node v _ _) = v

depth :: Tree a -> Integer
depth Empty = 0
depth (Node _ left right) = 1 + max (depth left) (depth right)

getLeaves :: Tree a -> [a]
getLeaves Empty = []
getLeaves (Node value Empty Empty) = [value]
getLeaves (Node _ left right) = getLeaves left ++ getLeaves right

-- data Tree a = Node a (Tree a) (Tree a) | Empty deriving Show
data Direction = L | R
data Crumb a = Crumb Direction a (Tree a)
type Breadcrumbs a = [Crumb a]
type Iterator a = (Tree a, Breadcrumbs a)

goLeft :: Iterator a -> Iterator a
goLeft (Empty, b) = (Empty, b) 
goLeft (Node v left right, b) = (left, (Crumb L v right) : b)

goRight :: Iterator a -> Iterator a
goRight (Empty, b) = (Empty, b)
goRight (Node v left right, b) = (right, (Crumb R v left) : b)

goUp :: Iterator a -> Iterator a
goUp (t, []) = (t, [])
goUp (curr, ((Crumb L v other):b)) = ((Node v curr other), b)
goUp (curr, ((Crumb R v other):b)) = ((Node v other curr), b)

modify :: (a -> a) -> Iterator a -> Iterator a
modify f (Node v left right, b) = (Node (f v) left right, b)
modify f (Empty, b) = (Empty, b)

makeTreeIterator :: Tree a -> Iterator a
makeTreeIterator t = (t, [])

topMost :: Iterator a -> Iterator a
topMost (t, []) = (t, [])
topMost iterator = topMost $ goUp iterator

-- leftMost :: Iterator a -> Iterator a