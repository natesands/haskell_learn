-- Nate Sands
-- 6 May 2020
-- CSc 51003
-- Prof. Troeger

-- PROBLEM SET 7 pt. 5


{- Developing Scheme-Like Lists and Procedures -}

-- Representation of nested lists w/ the following functions:

-- cdr, cons, car
-- length, listRef
-- myLast, myAppend
-- snoc, myReverse, deepReverse
-- makeTree, countLeaves
-- treeMap

-- contains instance of Show typeclass which displays lists in Scheme format

data List a = Empty | Atm a | List {car::List a, cdr::List a}

cons :: List a -> List a -> List a
cons l1 l2 = List { car = l1, cdr = l2 }

l1 = cons (Atm 2) Empty -- (2)
l2 = cons Empty l1  -- (() 2)
-- (1 2 3 4)
l3 = cons (Atm 1) (cons (Atm 2) (cons (Atm 3) (cons (Atm 4) Empty)))
-- (1 ((2)) 3)
l4 = cons (Atm 1) (cons (cons (cons (Atm 2) Empty) Empty) (cons (Atm 3) Empty))
l5 = cons l2 l3
l6 = cons (cons (Atm 2) Empty) Empty

{-
Instance of Show that displays (List a) in Scheme list format.
e.g.

λ> cons (Atm 1) (cons (cons (cons (Atm 2) Empty) Empty) (cons (Atm 3) Empty))
(1 ((2)) 3)

-}
instance (Show a) => Show (List a) where
  show x
    | isAtm x = showAtm x
    | isNull x = "()"
    | isSingleton x = "(" ++ (show (car x)) ++ ")"
    | otherwise = "(" ++ show (car x)  ++ rest (cdr x)
    where rest y
            | isNull y = ")"
            | isAtm (car y) =  " " ++ show (car y)  ++ rest (cdr y)
            | isPair (car y) = " " ++ show (car y) ++ rest (cdr y)
            | isNull (car y) = " " ++ "()" ++ rest (cdr y)
            | isSingleton (car y) = show (car y) ++ rest (cdr y)
          showAtm (Atm z) = show z
          
len :: List a -> Int
len Empty = 0
len lst = 1 + (len $ cdr lst)

isAtm :: List a -> Bool
isAtm (Atm x) = True
isAtm _ = False

isSingleton :: List a -> Bool
isSingleton (List {car= (Atm _), cdr = Empty}) = True
isSingleton _ = False

-- NOTE: ((2)) is a Scheme pair!! 
isPair :: List a -> Bool
isPair x
  | isSingleton x || isNull x || isAtm x = False
  | otherwise = True

isNull :: List a -> Bool
isNull Empty = True
isNull _ = False

listRef n l
  | n == 1 = car l
  | otherwise = listRef (n-1) (cdr l)

myLast :: List a -> List a
myLast l
  | isNull (cdr l) = car l
  | otherwise = myLast (cdr l)

myAppend :: List a -> List a -> List a
myAppend l m
  | isNull l = m
  | otherwise = cons (car l) (myAppend (cdr l) m)

-- linear reverse
myReverse :: List a -> List a
myReverse l = iter l Empty
  where iter :: List a -> List a -> List a
        iter l1 l2
          | isNull l1 = l2
          | otherwise = iter (cdr l1) (cons (car l1) l2)

snoc :: List a -> List a -> List a
snoc x lst
  | isNull lst = cons x lst
  | otherwise = (cons (car lst) (snoc x (cdr lst)))

deepReverse :: List a -> List a
deepReverse l
  | isNull l = Empty
  | isAtm (car l) = snoc (car l) (deepReverse (cdr l))
  | otherwise = snoc (deepReverse (car l)) (deepReverse (cdr l))

{-
λ> t5
(((1 2) 3 4) (2 3) 1)
λ> deepReverse t5
(1 (3 2) (4 3 (2 1)))
-}

makeTree :: [List a] -> List a 
makeTree [] = Empty
makeTree (x:xs) = cons x (makeTree xs) 

t1 = cons (Atm 1) (cons (Atm 2) Empty)
t2 = cons (Atm 3) (cons (Atm 4) Empty)
{-
λ> t1
(1 2)
λ> t2
(3 4)
λ> makeTree [t1,t2]
((1 2) (3 4))
-}

countLeaves :: List a -> Int
countLeaves tree
  | isNull tree = 0   -- List not Empty and not pair = Atm
  | not (isPair tree) = 1 
  | otherwise = (countLeaves (car tree)) + (countLeaves (cdr tree))

t3 = cons (Atm 1) Empty 
t4 = cons (cons (Atm 2) (cons (Atm 3) Empty )) t3
t5 = cons (cons t1 t2) t4
t6 = cons l3 t5
{-

λ> t5
(((1 2) 3 4) (2 3) 1)
λ> countLeaves t5
7
λ> t6
((1 2 3 4) ((1 2) 3 4) (2 3) 1)
λ> countLeaves t6
11

-}

-- retrieves Atm value
getAtm :: List a -> a
getAtm (Atm x) = x

-- applies a function to tree elements while keeping tree structure
treeMap :: ( a -> a ) -> List a -> List a
treeMap op t
  | isNull t = Empty
  | isAtm (car t)  = cons (Atm (op (getAtm (car t)))) (treeMap op (cdr t))
  | otherwise = cons (treeMap op (car t)) (treeMap op (cdr t))

{-

λ> treeMap (*2) t5
(((2 4) 6 8) (4 6) 2)
λ> t6
((1 2 3 4) ((1 2) 3 4) (2 3) 1)
λ> treeMap (\ x -> x^2 + 3*x + 2) t6
((6 12 20 30) ((6 12) 20 30) (12 20) 6)

-}

