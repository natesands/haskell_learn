
import Data.Typeable

infixr 5 :.:
data Pair a b = NullPair | (:.:) a b deriving (Read, Eq, Ord)

instance (Show a, Show b) => Show (Pair a b) where
  show NullPair = "()"
  show ((:.:) x y) = "(" ++ show x ++ " . " ++ show y ++ ")" 

--}
--Represent the list (1 ( ((2)) (3 () 4) ((5) 6) ))

-- ((2))
l1 = (2 :.: NullPair) :.: NullPair
-- (3 () 4)
l2 = 3 :.: NullPair :.: 4 :.: NullPair
-- ((5) 6)
l3 = (5 :.: NullPair) :.: 6 :.: NullPair
-- (1 ( ((2)) (3 () 4) ((5) 6) ))
ll = 1 :.: ( l1 :.: (l2 :.: (l3 :.: NullPair))) :.: NullPair

class Null t where
  isNull :: t -> Bool

instance Null [a] where
  isNull [] = True
  isNull _ = False

instance Null (Pair a b) where
  isNull NullPair = True
  isNull _ = False
{-
I would like to implement something like Scheme's pair?
... ??
-}

car :: Pair a b -> a
car NullPair = error "(:.:) x y expected"
car ((:.:) x y) = x

cdr :: Pair a b -> b
cdr NullPair = error "(:.:) x y expected"
cdr ((:.:) x y) = y


{-
len :: Pair a (Pair a b) -> Int
len NullPair = 0
len pair = len (cdr pair)
-}
{-
len pair = let c = cdr pair in
  if c == NullPair then 0
  else (1 + len c)
-}
{-
len NullPair = 0
len ((:.:) x y)
  | typeOf y == typeOf ((:.:) x y) = 1 + len y
--this version of length does not work
-}
{-
len NullPair = 0
len ((:.:) x y) = 1 + (len y) 
{-
len is expecting a type of Pair a b, but cdr returns
type b... this might be a pair, but haskell won't allow it.

-}

--- List as a tree object

data List a = Empty | Cons 


-}
