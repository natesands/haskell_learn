-- CHAPTER 11 --
import Data.Char
import Data.List
import Control.Applicative
{-
Recall the functor typeclass:

class Functor f where
  fmap :: (a -> b) -> f a -> f b

f has to have kind (* -> *)

So a type constructor f takes two parameters, we have to partially apply it, like...


class Functor (Either a) where
  fmap :: (b -> c) -> Either a b -> Either a c

-}

{- IO action is an instance of Functor.

instance Functor IO where
  fmap f action = do
       result <- action
       return (f result)

-}

main01 = do line <- getLine
            let line' = reverse line
            putStrLn $ "You said " ++ line' ++ " backwards!"
            putStrLn $ "Yes, you really said " ++ line' ++ " backwards!"



-- or

main01B = do
  line <- fmap reverse getLine
  putStrLn $ "you said, etc " ++ line

{-  In this case, fmap :: (a -> b) -> IO a -> IO b -}


main02 = do line <- fmap (intersperse '-' . reverse . map toUpper) getLine
            putStrLn line
            
{-

(->) is a type constructor that takes two arguments.

(->) r is an instance of Functor.

instance Functor ((->) r) where
  fmap f g = (\x -> f (g x))

which we might also denote as

instance Functor (r ->) where
fmap f g = (\x -> f (g x))

fmap has type (a -> b) -> f a -> f b

So in this case, we have

fmap :: (a -> b) -> (r -> a) -> (r -> b)

so fmap takes a function from r -> a, then composes it
with a function from a -> b, to get a function from r -> b

Another way to write this would be

instance Functor ((->) r) where
  fmap = (.)

Recall currying.  All functions in Haskell only take one
parameter.

if we write
fmap :: (a -> b) -> (f a -> f b)

we can say that fmap takes a function which returns a new
function that takes a functor as a parameter and returns
another functor.  i.e. it takes an (a -> b) function and
returns an (f a -> f b) function.  This is called 'lifting'
a function.

λ> :t fmap (*3) (+100)
fmap (*3) (+100) :: Num b => b -> b
λ> :t fmap (*2)
fmap (*2) :: (Functor f, Num b) => f b -> f b
λ> : fmap (replicate 3)
fmap (replicate 3) :: Functor f => f a -> f [a]

Two views of fmap:
1) a function that takes a function and a functor
   and then maps that function over the functor
2) a function that takes a function and lifts
  that function so it operates on functors.  


Functors should reliably behace as things that can be
mapped over.

FUNCTOR LAWS

1.  fmap id = id

e.g.
instance Functor Maybe
  fmap f (Just x) = Just (f x)
  fmap f Nothing = Nothing

2. fmap (f . g) = fmap f . fmap g

i.e. for any functor F

fmap (f.g) F = fmap f (fmap g F)

composing two functions and then mapping the resulting function
over a functor should be the same as first mapping one function
over the functor and then mapping the other one. 

Check Maybe...

fmap (f . g) Just x = Just (f . g x)
fmap f (fmap g Just x) = fmap f (Just (g x))
= Just (f (g (x)))

-}

{-
*** Applicative functors ***



We can map a function over a list of functions
λ> let a = fmap (*) [1,2,3,4]
λ> :t a
a :: Num a => [a -> a]
λ> fmap (\f -> f 9) a
[9,18,27,36]

How do we take Just (3 *) and apply it to Just 5 to get (Just 15).  
-}

apply :: Maybe (a -> b) -> Maybe a -> Maybe b
apply (Just f) (Just x) = Just (f x)
appky x Nothing = Nothing


{-
λ> apply (Just (* 3)) (Just 5)
Just 15λ> apply (Just (* 3)) (Just 5)
Just 15
-}

{-
The Applicative typeclass is defined in Control.Applicative

The Applicative typeclass defines two methods:
pure and <*>

class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

If a type constructor is of the Applicative typeclass, it
must be a Functor first.

pure takes a value of any type and returns an applicative functor with the value inside it.  It puts a value in a 'minimal context'

<*> take a function wrapped in a functor and maps it over the value contained in another functor.


instance Applicative Maybe where
  pure = Just
  Nothing <*> _ = Nothing
  (Just f) <*> something = fmap f something


λ> pure (* 5) <*> Just 7
Just 35

(Which is equivalent to)
fmap (* 5) (Just 7)

Note <*> is left associative

λ> pure (+) <*> Just 7 <*> Just 18
Just 25

pure f <*> x  equals fmap f x

pure puts a value in a 'default context'

Instead of writing pure f <*> x <*> y <*> ...

We could write fmap f x <*> y <*> ..

WHich is why Control.Applicative exports a function
<$> which is just the fmap as an infix operator.

(<$>) :: (Functor f) => (a->b) -> f a -> f b
f <$> x = fmap f x

(Note here the f in the function declaration signifies a functor, while the f in the defintion signifies the function (a->b))

instance Applicative [] where
  pure x = [x]
  fs <*> xs = [f x | f <- fs, x <- xs]

Note if we type
λ> :t pure 7
pure 7 :: (Applicative f, Num a) => f a

We lack some context...
(Note this is different than what the book has).

For lists, we have
<*> :: [a -> b] -> [a] -> [b]

the size of the lists involved do not need to be equal since this is implemented as a list comprehension:

λ> [(*3)] <*> [1,2,3]
[3,6,9]
λ> [(*3),(*2)] <*> [1,2,3]
[3,6,9,2,4,6]
λ> [] <*> [1,2,3]
[]

λ> [(+),(*)] <*> [1,2] <*> [3,4]
[4,5,5,6,3,4,6,8]
λ> (++) <$> ["ha", "heh", "hmm"] <*> ["?","!","."]
["ha?","ha!","ha.","heh?","heh!","heh.","hmm?","hmm!","hmm."]
λ>

λ> [x*y | x <- [2,5,10], y<- [8,10,11]]
[16,20,22,40,50,55,80,100,110]

is equivalent to...

λ> (*) <$> [2,5,10] <*> [8,10,11]
[16,20,22,40,50,55,80,100,110]

which demonstrates why

pure f <*> xs
is equivalent to
fmap f xs

** IO Instance **

Recall
class (Functor f) => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

instance Applicative IO where
  pure = return
  a <*> b = do
     f <- a
     x <- b
     return (f x)

the idea here is extraction and sequencing...

-}

myAction :: IO String
myAction = do
  a <- getLine
  b <- getLine
  return $ a ++ b

main03 = do
  a <- myAction
  putStrLn a
  
myAction' :: IO String
myAction' = (++) <$> getLine <*> getLine


{-

instance Applicative ((->) r) where
 pure x = (\_ -> x)
 f <*> g = = \x -> f x (g x)

when pure wraps a value into an applicative functor it has to stay that value.

e.g.

λ> :t (+) <$> (+3) <*> (*100)
(+) <$> (+3) <*> (*100) :: Num b => b -> b

λ> (+) <$> (+3) <*> (*100) $ 5
508


^^ ** THIS SECTION IS A LITTLE UNCLEAR FOR ME ** ^^


the ZipList instance of Applicative:

instance Applicative ZipList where
  pure x = ZipList (repeat x)
  ZipList fs <*> ZipList <xs> = ZipList (zipWith (\f x -> f x) fs xs)

ZipList ([(*3), (*2), (+1)]) <*> ZipList( [1,2,3])
ZipList {getZipList = [3,4,4]}
λ> getZipList $ (+) <$> ZipList [1,2,3] <*> pure 100
[101,102,103]
λ> getZipList $ max <$> ZipList [1,2,3,4,5] <*> ZipList [7,2,8,1,20]
[7,2,8,4,20]
λ> :t (,,) <$> ZipList "dog"
(,,) <$> ZipList "dog" :: ZipList (b -> c -> (Char, b, c))
λ> (,,) <$> ZipList "dog" <*> ZipList "cat" <*> ZipList "bird"
ZipList {getZipList = [('d','c','b'),('o','a','i'),('g','t','r')]}
λ>


liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

This maps a function over an applicative functor and then applies
it to another applicative functor.

It promotes a normal binary function to a function that operates on
two functors.

λ> liftA2 (:) (Just 3) (Just [4])
Just [3,4]


-}

sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' [] = pure []
sequenceA' (x:xs) = (:) <$> x <*> sequenceA xs

sequenceA'' :: (Applicative f) => [f a] -> f [a]
sequenceA''  = foldr (liftA2 (:)) (pure [])


-- e.g. applying a list of functions to the same input
{-
Just [3,4]
λ> sequenceA' $ map Just [3, 2, 1]
Just [3,2,1]
λ> sequenceA' [Just 3, Nothing, Just 1]
Nothing
λ> fmap (+) [3,2,1]
fmap (+) [3,2,1] :: Num a => [a -> a]
λ> sequenceA' [(+3),(+2),(+1)] 3
[6,5,4]
λ> sequenceA' [(>4),(<10),odd] 7
[True,True,True]
λ> and $ sequenceA [(>4),(<10),odd] 7
True
λ> sequenceA [[1,2,3],[4,5,6]]
[[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
λ> 

** LAWS OF APPLICATIVE FUNCTORS **

1.  f <*> x = fmap f x

2. pure id <*> v = v

3. pure (.) <*> u <*> v <*> w = u <*> (v <*>w)

4. pure f <*> pure x = pure (f x)

5. u <*> pure y = pure ($y) <*> u

-}

{- NEWTYPE -}

-- recall

lst01 = getZipList $ ZipList [(+1),(*100),(*5)] <*> ZipList [1,2,3]
-- [2, 200, 15]

data MyZipList a = MyZipList {getMyZipList :: [a]}

-- or

newtype MyZipList' a = MyZipList' { getMyZipList' :: [a] }

-- newtype allows us to simply wrap an existing type into a new type
-- it is like a data declaration that only has one constructor and one
-- field

-- how to make fmap apply a function to first coordinate of a tuple only?

newtype Pair b a = Pair { getPair :: (a,b) } deriving (Show)

instance Functor (Pair b) where
  fmap f (Pair (x,y)) = Pair (f x, y)
  
