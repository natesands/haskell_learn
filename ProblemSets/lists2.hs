
{-- Scheme-like Lists --}
{-
data Pair a = NullPair | Pair {thisCar :: a, thisCdr :: Pair a}

cons :: a -> Pair a -> Pair a
cons x pair = Pair { thisCar = x, thisCdr = pair } 

car :: Pair a -> 
-}
{-
data List a = Empty | List { car :: a, cdr :: List a } 

cons :: a -> List a -> List a
cons x lst = List {car = x, cdr = lst}

l1 = cons 2 Empty    -- (2)
--l2 = cons 3 (cons Empty (cons 4 Empty))  -- (3 () 4)
-- l3 = cons (cons 5 Empty) (cons 6 Empty)

instance (Show a) => Show (List a) where
  show Empty  = "()"
  show (List { car=x, cdr=y}) = "(" ++ show x ++ " . " ++ show y ++ ")" 

-}
{-
data List a = Empty | List { car :: a, cdr :: List a } | Nil (List a) 

cons :: a -> List a -> List a
cons Empty lst = Nil lst 
cons x lst = List {car = x, cdr = lst}

l1 = cons 2 Empty    -- (2)
--l2 = cons 3 (cons Nil (cons 4 Empty))
--l2 = cons 3 (cons Empty (cons 4 Empty))  -- (3 () 4)
-- l3 = cons (cons 5 Empty) (cons 6 Empty)

instance (Show a) => Show (List a) where
  show Empty  = "()"
  show (Nil lst) = "(() . " ++ show lst ++ ")"
  show (List { car=x, cdr=y}) = "(" ++ show x ++ " . " ++ show y ++ ")"
-}

{-
data List a = EndOfList | List { car :: a, cdr :: List a }
data Val a = Empty | Val a


cons :: a -> List (Val a) -> List (Val a)
cons x lst = List {car = (Val x), cdr = lst}

-- exclusively for adding Nil to a list
consNil :: List (Val a) -> List (Val a)
consNil lst = List {car = Nil, cdr = lst}


instance (Show a) => Show (List a) where
  show EndOfList = ""
  show (List { car = x, cdr=y}) = "(" ++ show x ++ " " ++ show y ++ ")"

instance (Show a) => Show (Val a) where
  show Nil = "()"
  show (Val a) = show a
-}


data List a = Empty | Lit a | List { car :: List a, cdr :: List a} | EndOfList

cons :: List a -> List a -> List a
cons (Lit x) lst = List { car = Lit x, cdr = lst}
cons EndOfList _ = error "invalid cons value"
cons lst1 lst2 = List { car = lst1, cdr = lst2}

{-
instance (Show a) => Show (List a) where
  show Empty = "()"
  show (Lit x) = show x
  show (List {car = x, cdr = EndOfList}) = "(" ++ show x ++ ")"
  show (List {car = x, cdr = y}) = "(" ++ show x ++ " " ++ show y ++ ")"
-}

instance (Show a) => Show (List a) where
  show (Lit x) = show x
  show EndOfList = ""
  show Empty = "()"
  show lst = "(" ++ (show $ car lst) ++ (show $ cdr lst) ++ ")"
  
len :: List a -> Int
len EndOfList = 0
len lst = 1 + (len $ (cdr lst))

listRef :: Int -> List a -> List a
listRef n l
  | n == 1 = car l
  | otherwise = listRef (n - 1) (cdr l)

l1 = cons (Lit 1) (cons (Lit 2) (cons (Lit 3) EndOfList))
l2 = cons (cons (Lit 1) (cons (Lit 2) EndOfList)) (cons (Lit 3) EndOfList)


{-
append :: List a -> List a -> List a
append l m
  | l == EndOfList
-}

{-
data List a = Empty | Lit a | List { car :: List a, cdr :: List a} deriving (Show)

cons :: List a -> List a -> List a
cons x l = List {car= x, cdr=l}

l1 = cons (cons (Lit 2) Empty) (cons (Lit 3) (cons (Lit 5) Empty))
l2 = List { car = List { car = Lit 2, cdr = Empty }, cdr = List {car = Lit 3, cdr = Empty}}
-}
{-
instance (Show a) => Show (List a) where
  show Empty = ""
  show (Lit x) = show x
  show (List {car = x, cdr = y}) = "(" ++ show x ++ " " ++ show y ++ ")"
-}
{-
instance (Show a) => Show (List a) where
  show Empty = "()"
  show (Lit x) = show x
  show (List {car = x, cdr = y}) = "(" ++ show x ++ " " ++ show y ++ ")"
-}
{-
instance (Show a) => Show (List a) where
  show (List {car = Empty, cdr = y}) = "()" ++ (show y)
  show (List {car = x, cdr = Empty}) = show x
  show (Lit x) = show x
  show (List {car =x, cdr =y}) = "(" ++ (show x) ++ " " ++ (show y) ++ ")"
-}
{-
instance (Show a) => Show (List a) where
  show Empty = "()"
  show (Lit x) = show x
  show (List {car=x, cdr=y}) =  (show x) ++ "." ++ (show y)
-}
{-
instance (Show a) => Show (List a) where
  show Empty = "()"
  show Lit
  show List {car = Empty, cdr = Empty} = "()"
  show List {car = Lit x, cdr = Empty} = show x
  show List {car = x, cdr = y} = "(" ++ (show x) ++ " "++  (show y)++  ")"
-}
