data Tree a = Empty | Node (a, Forest a) deriving (Show)
data Forest a = Nil | Cons (Tree a) (Forest a) deriving (Show)



