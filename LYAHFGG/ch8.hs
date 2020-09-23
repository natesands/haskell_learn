{- MYAHFGG Chapter 8 -}
module Shapes
  ( Point(..)
  , Shape(..)
  , surface
  , nudge
  , baseCircle
  , baseRectangle
  ) where

import qualified Data.Map as Map
 

-- (adding (..) is equivalent to Shape(Circle,Rectangle)
-- data Bool = True | False

-- type name and constructors must be in caps
{-
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)
-- Circle :: Float -> Float -> Float -> Shape


surface :: Shape -> Float
surface (Circle _ _ r) = pi*r^2
surface (Rectangle x1 y1 x2 y2) = (abs $ x1 - x2) * (abs $ y1 - y2)

-- Note, Shape is a type, Circle is not.

-- to print out st like Circle 10 20 10
-- we need to add deriving (Show) type to type definition -- 

-- Value constructors a are functions like anything else
concentrics :: [Float] -> [Shape]
concentrics xs = map (Circle 10 10) xs
-}
-- Point Value

data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _  r) = pi*r^2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x1 - x2) *
         (abs $ y1 - y2)

nudge :: Shape -> Float -> Float  -> Shape
nudge (Circle (Point x y) r) dx dy = (Circle (Point (x+dx) (y+dy)) r) 
nudge (Rectangle (Point x1 y1) (Point x2 y2)) dx dy =
       (Rectangle (Point (x1+dx) (y1+dy)) (Point (x2+dx) (y2+dy)))

baseCircle :: Float -> Shape
baseCircle r = (Circle (Point 0 0) r)

baseRectangle :: Float -> Float -> Shape
baseRectangle width height = (Rectangle (Point 0 0) (Point width height))

-- Record Syntax
{-
data Person = Person { firstName :: String
                     , lastName :: String
                     , age::Int
                     , height::Float
                     , phoneNumber::String
                     , flavor::String
                     } deriving (Show)
--  This creates a set of lookup functions, firstName, lastName, age, etc.
-}
data Car = Car { company :: String
               , model :: String
               , year :: Int
               } deriving (Show)

-- TYPE PARAMETERS
tellCar :: Car -> String
tellCar (Car { company = c, model = m, year = y}) = "This " ++ c ++ " " ++
  m ++ " was made in " ++ show y

-- "We usually use type parameters when the type that's contained
-- inside the data type's various constructors isn't really that
-- important for the type to work."

-- e.g. Map k v type from Data.Map

-- e.g.

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

scalarMult :: (Num t) => Vector t -> t -> Vector t
(Vector i j k) `scalarMult` m = Vector (i*m) (j*m) (j*m)

-- data <name> <type parameter> = <name> <value constructor> <value constructor>

-- DERIVED INSTANCES


data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     } deriving (Eq, Show, Read)

-- dumb example

data Capitals = W | X | Y | Z deriving (Eq, Show, Read, Ord, Bounded, Enum)

{-
λ> minBound :: Capitals
W
λ> maxBound :: Capitals
Z
λ> show Y
"Y"
λ> read "Y"
*** Exception: Prelude.read: no parse
λ> read "Y" :: Capitals
Y

λ> succ X
Y
λ> pred W
*** Exception: pred{Capitals}: tried to take `pred' of first tag in enumeration
CallStack (from HasCallStack):
  error, called at /home/nate/Documents/haskell/learn/ch8.hs:104:71 in main:Shapes
λ> pred Z
Y
λ> [1..3]
[1,2,3]
λ> [W .. Z]
[W,X,Y,Z]
λ> [minBound :: Capitals .. maxBound:: Capitals]
[W,X,Y,Z]
-}

-- TYPE SYNONYMS --

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber book = (name,pnumber) `elem` book

type AssocList k v = [(k,v)]

-- a type that represents a map from integers to something
type IntMap v = Map.Map Int v

-- or type IntMap = Map.Map Int

-- data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
  case Map.lookup lockerNumber map of 
    Nothing -> Left $ "Locker number " ++ show lockerNumber ++ " doesn't exist!"
    Just (state, code) -> if state /= Taken
                          then Right code
                          else Left $ "Locker " ++ show lockerNumber ++
                               " is already taken!"

lockers :: LockerMap
lockers = Map.fromList
  [(100, (Taken, "a"))
  ,(101, (Free, "b"))
  ,(102, (Free, "c"))
  ,(105, (Taken, "z"))
  ]

-- this construction allows us to get information about why an
-- error occured

-- RECURSIVE DATA STRUCTURES

data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

-- operators composed of only special characters are automaticall defined
-- to be infix operators
-- define :-: to have fixity 5, and be right associative.  i.e.
-- 3 :-: 4 :-: 5 :-: 6 :-: 7 = (3 :-: (4 :-: ( 5 :-: (6 :-: 7))))
infixr 5 :-:
data List' a = Empty' | (:-:) a (List' a) deriving (Show, Read, Eq, Ord)


{- how ++ is defined

infixr 5 ++
(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : (xs ++ ys)
-}

infixr 5 .++
(.++) :: List' a -> List' a -> List' a
Empty' .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys) 

-- the pattern matching on the last line works because pattern matching
-- is about matching constructors

-- Trees


data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show)

{-
root = Node {3 EmptyTree EmptyTree}
-}


root = Node 3 EmptyTree EmptyTree

singleton :: a -> Tree a
singleton a = Node a EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
  | x == a = Node a left right
  | x < a  = Node a (treeInsert x left) right
  | x > a  = Node a left (treeInsert x right)

val :: Tree Int -> Int
val (Node a left right) = a

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
  | a == x = True
  | a < x = treeElem x right
  | a > x = treeElem x left
  
treeMax :: (Eq a) => Tree a -> a
treeMax (Node a left right)
  | right == EmptyTree = a
  | otherwise = treeMax right

treeMin :: (Eq a) => Tree a -> a
treeMin (Node a left right)
  | left == EmptyTree = a
  | otherwise = treeMin left

aTree :: Tree Int
aTree = (Node 10 (Node 4 (Node 3 EmptyTree EmptyTree) (Node 6 EmptyTree EmptyTree)) (Node 5 EmptyTree (Node 8 EmptyTree EmptyTree)))

-- Fill a tree using a fold
fillTree :: (Ord a) => [a] -> Tree a
fillTree xs = foldr (\x tree -> treeInsert x tree) EmptyTree xs

numTree = fillTree [3,5,2,4,1,56,4,5] 
charTree = fillTree "hello"

-- TYPECLASSES 102 --

{-
how Eq typeclass is defined

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  x == y = not (x /= y)
  x /= y = not (x == y)

-}


data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red == Red = True
  Yellow == Yellow = True
  Green == Green = True
  _ == _ = False


-- Note by declaring a function in a class
-- e.g. foo :: a -> a -> a
-- without defining it, we force the instance
-- algebraic type to define the function 

instance Show TrafficLight where
  show Red = "Red Light"
  show Yellow = "Yellow Light"
  show Green = "Green Light"

{-
λ> Red == Red
True
λ> Red == Yellow
False
λ> Red `elem` [Red, Yellow, Yellow, Green]
True
λ> show [Red, Yellow, Green]
"[Red Light,Yellow Light,Green Light]"
-}

instance (Eq a) => Eq (Tree a) where
  (Node a aleft aright) == (Node b bleft bright) = a == b
  _ == _ = False
  
instance (Ord a) => Ord (Tree a) where
  treeA <= treeB =  (treeMax treeA) <= (treeMax treeB)
  treeA < treeB =  (treeMax treeA) < (treeMax treeB)
  treeA > treeB =  (treeMax treeA) > (treeMax treeB)
  treeA >= treeB =   (treeMax treeA) >=  (treeMax treeB)

-- A YES-NO TYPECLASS

class YesNo a where
  yesno :: a -> Bool

instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno (Just _) = True
  yesno Nothing = False

instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _ = True

instance YesNo (TrafficLight) where
  yesno Red = False
  yesno _ = True
  
--- NOTE: yesno 0 returns an error, but yesno (0::Int) does not
--- see stackoverflow question haskell-yesno-type-class-why-integer
instance YesNo Integer where
  yesno 0 = False
  yesno _ = True

yesnoIf :: (YesNo y) => y -> a -> a -> a
yesnoIf yesnoVal yesResult noResult = if yesno yesnoVal then yesResult else noResult


-- FUNCTOR TYPECLASS

{-
class Functor f where
  fmap :: (a -> b) -> f a -> f b

Here f is not a concret type, but a type constructor that takes one type parameter

fmap takes a function from one type to another and a functor
applied with one type are returns a functor applied with another
type

e.g.
map :: (a -> b) -> [a] -> [b]
takes a function from one type to another, a List of one type, and a List of another type

map is an fmap that works only on lists

instance Functor [] where
  fmap = map


[] is a type constructor:  [Int], [String], [[String]] ar concrete types

instance Functor Maybe where
  fmap f (Just x) = Just (f x)
  fmap f Nothing = Nothing

So for the type of Maybe, fmap works like

fmap :: (a -> b) -> Maybe a -> Maybe b

-}

instance Functor Tree where
  fmap f EmptyTree = EmptyTree
  fmap f (Node x leftsub rightsub) =
    Node (f x) (fmap f leftsub) (fmap f rightsub)

{-
λ> aTree
Node 10 (Node 4 (Node 3 EmptyTree EmptyTree) (Node 6 EmptyTree EmptyTree)) (Node 5 EmptyTree (Node 8 EmptyTree EmptyTree))
λ> 
λ> fmap yesno aTree
Node True (Node True (Node True EmptyTree EmptyTree) (Node True EmptyTree EmptyTree)) (Node True EmptyTree (Node True EmptyTree EmptyTree))
-}

{-
Recall that the Either type (in Data.Either) is
data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)

instance Functor (Either a) where
  fmap f (Right x) = Right (f x)
  fmap f (Left x) = Left x

Notice that (Either a) is a type constructer that takes one parameter
so fmap here has signature
fmap :: (b -> c) -> (Either a) b -> (Either a) c


fmap for Map k v?

instance Functor (Map k) where
  fmap f (Map k v) = Map k (f v)
-}


-- Kinds and some type-foo

-- type : value :: kind type

{-
λ> :k Int
Int :: *
λ> :k Tree
Tree :: * -> *
λ> :k Tree Int
Tree Int :: *
λ> :k Maybe
Maybe :: * -> *
λ> :k Either
Either :: * -> * -> *
λ> :k Either Int
Either Int :: * -> *
-}

-- * means a the type is a concrete type.  A concerete type is a type
-- that doesn't take any type parameters and calues con only havce
-- types that are concrete types

-- Functor takes types of kind * -> *
{-
class Functor f where
  fmap :: (a -> b) -> f a -> f b

this shows that f has to produce a concrete type, since it is
used a a type of value in a function.  So f must be of kind * -> *
-}

-- e.g.

class Tofu t where
  tofu :: j a -> t a j

-- what kind would t be? j a is a concrete value, so j must be of kind
-- * -> *.  a is of kind *.  So t is of kind * -> (* -> *) -> *

-- so we create a type of this kind

data Frank a b = Frank {frankField:: b a} deriving (Show)

-- we assume a is kind *, then b is of kind (* -> *), so the kind
-- of Frank is * -> (* -> *) -> *

{-
λ> :t Frank {frankField = Just "HAHA"}
Frank {frankField = Just "HAHA"} :: Frank [Char] Maybe
λ> :t Frank {frankField = Node 'a' EmptyTree EmptyTree}
Frank {frankField = Node 'a' EmptyTree EmptyTree}
  :: Frank Char Tree
λ> :t Frank {frankField = ['Y','E','S']}
Frank {frankField = ['Y','E','S']} :: Frank Char []
^^^ Note that if we were to use our own List type
this would be of type Frank List Char
-}

instance Tofu Frank where
  tofu x = Frank x

-- tofu takes a (j a) with kind * 

{-
λ> tofu (Just 'a') :: Frank Char Maybe
Frank {frankField = Just 'a'}
λ> tofu "HELLO" :: Frank Char []
Frank {frankField = "HELLO"}
λ> tofu ["HELLO"] :: Frank [Char] []
Frank {frankField = ["HELLO"]}

-}

data Barry t k p = Barry { yabba :: p, dabba :: t k}
-- hsd kind (* -> *) -> * -> * -> *

-- To make this a part of functor, we have to make it a type of kind
-- * -> *

instance Functor (Barry a b) where
  fmap f (Barry {yabba=x, dabba=y}) = Barry {yabba = f x, dabba = y}
