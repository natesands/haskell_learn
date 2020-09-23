-- Nate Sands
-- 5 May 2020

{-

CDR/CAR TREES

The idea here is to represent Scheme-like lists as binary trees, where the right subtree represents the car, and the left subtree represents the cdr.

This might be more trouble than it's worth.

(See the attached pdf files for the structure of the representation and examples.)


-}

data Tree a = Nil | Node a (Tree a) (Tree a) | Nest (Tree a) (Tree a) deriving (Show)


-- ()
t0 = Nil
-- (1)
t1 = Node 1 Nil Nil
-- (1 2)
t2 = Node 1 (Node 2 Nil Nil) Nil
-- (1 (2 3))
t3 = Node 1 (Node 2 Nil (Node 3 Nil Nil)) Nil
-- ((1 2) 3)
t4 = Node 1 (Node 3 Nil Nil) (Node 2 Nil Nil)
-- (1 2 (3 4)))
t5 = Node 1 (Node 2 (Node 3 Nil (Node 4 Nil Nil)) Nil) Nil
-- (1 2 3)
t6 = Node 1 (Node 2 (Node 3 Nil Nil) Nil) Nil
-- ((1 2 3 4) 5)
t7 = Node 1 (Node 5 Nil Nil) (Node 2 Nil (Node 3 Nil (Node 4 Nil Nil))) 

-- DIFFICULTY: Unable to represent () as an element of a list... unless we use the Nest constructor, which also alows us more than one level of nested lists.

-- (())
tt0 = Nest Nil Nil
-- (1 ())
tt3 = Node 1 (Nest Nil Nil) Nil
-- ((1))
tt1 = Nest Nil (Node 1 Nil Nil)
--(((1 2)))
tt2 = Nest Nil (Node 1 Nil (Node 2 Nil Nil)) 


len :: Tree a -> Int
len Nil = 0
len (Nest Nil right) = len right
len (Node x left right) = 1 + (len left)

{-
car, cdr, cons versions before adding Nest constructor..
Need to update these..

We need definitions of car and cdr that preserve the invariant structure (i.e., the left subtree is the cdr, and the right subtree (root inclusive) is the car.

cdr seems to work alright.

car requires some finessing so that repeated applications of car give the right results.

Here car takes the right subtree and moves it to the left.

Need to prove that this is correct.


car :: Tree a -> Tree a
car (Node x left right) = Node x right Nil

cdr :: Tree a -> Tree a
cdr (Node x left right) = left

cons (Node x Nil _) (Node y l r) = Node x (Node y l r) Nil
cons (Node x left _) tree = cons left tree

-}


