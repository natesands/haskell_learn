
import Control.Monad as Mnd
{- CHAPTER 12 -}

{- Monads -}

{-

recall

fmap :: (Functor f) => (a -> b) -> f a -> f b

(<*>) :: (Applicative f) => f (a -> b) -> f a -> f b
pure (wraps a value in a datatype)

How do we apply a function  a -> m b to a value with context m a?

(>>=) :: (Monad m) => m a -> (a -> m b) -> m b

Monads are applicative functors that support >>= (bind)

case with Maybe ...
-}

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f  = f x

{- Monad Typeclass

class Monad m where
  return :: a -> m a
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  x >> y = x >>= \_ -> y
  fail msg = error msg


note that even though it is not specified, m should be an instance of an applicative
functor.

Maybe as an instance of Monad type class

instance Monad Maybe where
  return x = Just x
  Nothing >>= f = Nothing
  Just x >>= f = f x
  fail _ = Nothing
-}

-- Tight rope walker and birds

type Birds = Int
type Pole = (Birds, Birds)
{-
landLeft :: Birds -> Pole -> Pole
landLeft x (left, right) = (left + x, right)

landRight :: Birds -> Pole -> Pole
landRight n (left, right) = (left, right + n) -}

-- Now revise this

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs(left + n - right) < 4 = Just (left + n, right)
  | otherwise = Nothing
  
landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs(right + n - left) < 4 = Just (left, right +n)
  | otherwise = Nothing

-- we want to be able to chain these together as in
-- landRight 1 (landLeft 2 (landLeft -3 (10,1)

test = landRight 1 (0,0) >>= landLeft 3

{- note that failure propogates.. -}

test2 = Nothing >>= landLeft 3  -- gives Nothing

test3 = return (0, 0) >>= landRight 2 >>= landLeft 2 >>= landRight 2 -- (2,4)

test4 = return (0,0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)

banana :: Pole -> Maybe Pole
banana _ = Nothing

test5 = return (0,0) >>= landRight 1 >>= landLeft 3 >>= banana >>= landRight 2

-- the >> operator
{-
(>>) :: (Monda m) => m a -> m b -> m b
m >> n = m >>= \_ -> n
-}


test6 = return (0,0) >>= landLeft 1 >> Nothing >>= landRight 1
-- Nothing

{-
The Maybe monad saves us a lot of time when we have to
successively do computations that are based on computations
that might have failed.
-}

{-
The bind operator >>= preserves the context of the value to
which it applies functions.
-}

{- "do" Notation -}
{-
λ> Just 3 >>=  (\x -> Just (show x ++ "!"))
Just "3!"
λ> Just 3 >>= (\x -> Just "!" >>= (\y -> Just(show x ++ y)))
Just "3!"
-}

foo :: Maybe String
foo = Just 3 >>= (\x ->
                     Just "!" >>= (\y ->
                                     Just (show x ++ y)))
      

-- or, us do notations

foo' :: Maybe String
foo' = do
  x <- Just 3
  y <- Just "!"
  Just (show x ++ y)
  
-- the effect is to extract things from Maybe values
-- do expressions are just different syntax for chaining monadic
-- values
foo'' :: Maybe String
foo'' = do
  x <- Just 3
  z <- Nothing
  y <- Just "!"
  Just (show x ++ y)

-- note this evaluates to Nothing
routine' :: Maybe Pole
routine' = do
  start <- return(0,0)
  first <- landLeft 2 start
  second <- landRight 2 first
  landLeft 1 second

-- '<-' extracts the value from its monadic context

-- pattern matching in a do expression

justH :: Maybe Char
justH = do
  (x:xs) <- Just "Hello"
  return x

-- when pattern matching fails in a do expression, the fail function is
-- called

{- default impl.:
fail :: (Monad m) => String -> m a
fail msg = error msg

for Maybe, it is implemented as
fail _ = Nothing
-}

wopwop :: Maybe Char
wopwop = do
  (x:xs) <- Just ""
  return x

{- THE LIST MONAD -}
-- Maybe monads are values with a failure context;  list monads bring in non-determinism
{-
λ> (*) <$> [1,2,3] <*> [10,100,1000]
[10,100,1000,20,200,2000,30,300,3000]
-}

{- Monad instance of list

instance Monad [] where
  return x = [x]
  xs >>= f = concat (map f xs) 
  fail _ = [] -}
  
-- recall that concat flattens a list of lists into a list
-- and... (>>=) :: Monad m => m a -> (a -> m b) -> m b

{-
λ> [3,4,5,6] >>= (\x -> [x,-x])
[3,-3,4,-4,5,-5,6,-6]
-}
-- [] functions like Nothing
{-
λ> [] >>= \x -> ["ay","bee","see"]
[]
λ> [1,2,3] >>= \x -> []
[]
-}

-- chaining with >>= propogates non-determinsm
 {-
λ> [1,2] >>= \n -> ['a','b'] >>= \ch -> return (n,ch)
[(1,'a'),(1,'b'),(2,'a'),(2,'b')]

why does this work?
-}
-- equivalently
listOfTuples :: [(Int,Char)]
listOfTuples = do
  n <- [1,2]
  ch <- "ab"
  return (n,ch)

-- this is equivalent to !!
listOfTuples2 = [(n,ch) | n <- [1,2], ch <- "ab"]

-- list comprehensions are syntactic sugar for using lists as monads

-- MonadPlus class
{-
class Monad m => MondaPlus m where
  mzero :: m a  ---> corresponds to mempty
  mplus :: m a -> m a -> m a  ---> corresponds to mappend


lists are monads and monoids

instance MonadPlus [] where
  mzero = []
  mplus = (++)
-}
{- guard from Control.Monad

guard :: (MonadPlus m) => Bool -> m ()
guard True = return ()
guard False = mzero

λ> 
λ> Mnd.guard (5 > 2) :: Maybe ()
Just ()
λ> Mnd.guard (5 > 2) :: [()]
[()]
λ> Mnd.guard (5 > 2) :: Maybe ()
Just ()
λ> Mnd.guard (1 > 2) :: Maybe ()
Nothing
λ> Mnd.guard (5 > 2) :: [()]
[()]
λ> Mnd.guard (1 > 2) :: [()]
[]
λ> [1..50] >>= (\x -> Mnd.guard (elem '7' (show x)) >> return x)
[7,17,27,37,47]
λ> Mnd.guard (5 > 2) >> return "cool" :: [String]
["cool"]
λ> Mnd.guard (1 > 2) >> return "cool" :: [String]
[]

If guard receives a true value, it returns an empty tuple within the context.
This is ignored by >> and a result is returned.

If false, then it returns mzero which translates to a failed computation.
>>= returns the 'Nothing' value.
-}

-- we can rewrite this as

sevensOnly :: [Int]
sevensOnly = do
  x <- [1..50]
  Mnd.guard ('7' `elem` show x)
  return x


{- A knight's quest -}

type KnightPos = (Int, Int)
-- generate all possible moves from (c,r)
moveKnight :: KnightPos -> [KnightPos]
moveKnight (c,r) = do
  (c',r') <- [(c+2,r-1),(c+2,r+1), (c-2,r-1), (c-2,r+1)
              ,(c+1,r-2), (c+1,r+2),(c-1,r-2), (c-1,r+2)
              ]
  Mnd.guard (c' `elem` [1..8] && r' `elem` [1..8])
  return (c',r')

-- find all the locations it can move in three jumps

in3 :: KnightPos -> [KnightPos]
in3 start = do
  first <- moveKnight start
  second <- moveKnight first
  moveKnight second

--or

in3' :: KnightPos -> [KnightPos]
in3' start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start finish = finish `elem` in3 start
