{--- Following explanation of Rose Tress in Bird, "Introduction to Functional Programming using Haskell"

data Rose a = Node a [Rose a] deriving (Show)


--  (1,2,3)
l1 = Node 1 [Node 2 [ Node 3 []]]
-- (1,(2,3))

-}

data Tree a = Null | Fork a (Tree a) (Tree a) deriving (Show)

-- ()
l0 = Null
-- (1)
l1 = Fork 1 (Null) (Null)
-- (1 2)
l2 = Fork 1 (Null) (Fork 2 Null Null)
-- THE MOST PROBLEMATIC ELEMENT IS HOW TO REPRESENT THE EMPTY SET AS AN ELEMENT OF A LIST
(1 (2 3))
l1a2 = Fork 1 (Null) (Fork 
