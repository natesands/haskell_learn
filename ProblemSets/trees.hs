{-
Paradigms:

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)

data Tree a = Empty | Node (a, Forest a) deriving (Show)
data Forest a = Nil | Cons (Tree a) (Forest a) deriving (Show)
-}




data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show,Eq)

root = Node 3 Empty Empty

singleton :: a -> Tree a
singleton a = Node a Empty Empty

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x Empty = singleton x
treeInsert x (Node a left right)
  | x == a = Node a left right
  | x < a  = Node a (treeInsert x left) right
  | x > a  = Node a left (treeInsert x right)

val :: Tree Int -> Int
val (Node a left right) = a

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x Empty = False
treeElem x (Node a left right)
  | a == x = True
  | a < x = treeElem x right
  | a > x = treeElem x left
  
treeMax :: (Eq a) => Tree a -> a
treeMax (Node a left right)
  | right == Empty = a
  | otherwise = treeMax right

treeMin :: (Eq a) => Tree a -> a
treeMin (Node a left right)
  | left == Empty = a
  | otherwise = treeMin left

aTree :: Tree Int
aTree = (Node 10 (Node 4 (Node 3 Empty Empty) (Node 6 Empty Empty)) (Node 5 Empty (Node 8 Empty Empty)))

-- Fill a tree using a fold
fillTree :: (Ord a) => [a] -> Tree a
fillTree xs = foldr (\x tree -> treeInsert x tree) Empty xs

numTree = fillTree [3,5,2,4,1,56,4,5] 
charTree = fillTree "hello"

collapse :: Tree a -> [a]
collapse Empty = []
collapse (Node x Empty Empty) = [x]
collapse (Node x left right) = (collapse left) ++ [x] ++ (collapse right)
