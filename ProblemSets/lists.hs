
{- Hi Professor --
I'm getting stuck trying to implement Scheme-like lists.

Here is my latest attempt, using a Pair data structure:
-}

data Pair a b =  NullPair | Pair { thisCar :: a, thisCdr :: b} deriving (Eq)


setCar :: Pair a b -> a -> Pair a b
setCar (Pair {thisCar = x, thisCdr = y}) newCar = Pair {thisCar = newCar, thisCdr = y}

setCdr :: Pair a b -> b -> Pair a b
setCdr (Pair {thisCar = x, thisCdr = y}) newCdr = Pair {thisCar = x, thisCdr = newCdr}

cons :: a -> b -> Pair a b
cons x y = Pair { thisCar = x, thisCdr = y}

car :: Pair a b -> a
car (Pair {thisCar = x, thisCdr = y}) = x

cdr :: Pair a b -> b
cdr (Pair {thisCar = x, thisCdr = y}) = y

-- Using this, we can define a list to be a pair whose innermost cdr is the Nil Pair

l1 = NullPair   -- ()
l2 = cons 3 NullPair  -- (3)
l3 = cons (cons 2 NullPair) (cons 3 (cons 4 NullPair))  -- ((2) 3 4)

instance (Show a, Show b) => Show (Pair a b) where
  show NullPair = "()"
  show (Pair { thisCar=x, thisCdr=y}) = "(" ++ show x ++ " . " ++ show y ++ ")" 

{-
Here is the output in Scheme pair notation

λ> l1
()
λ> l2
(3 . ())
λ> l3
((2 . ()) . (3 . (4 . ())))

Car and cdr...

λ> car l2
3
λ> car l3
(2 . ())
λ> cdr l2
()
λ> cdr l3
(3 . (4 . ()))
λ> cdr (cdr l3)
(4 . ())
λ> 

The problem arises when you attempt to define a length function in the usual recursive way:

len :: Pair a b -> Integer
len NullPair = 0
len (Pair {thisCar = x, thisCdr = y}) = 1 + len y

You get this error (I'll offer my interpretation after):

lists.hs:70:49: error: …
    • Couldn't match expected type ‘Pair a0 b0’ with actual type ‘b’
      ‘b’ is a rigid type variable bound by
        the type signature for:
          len :: forall a b. Pair a b -> Integer
        at /home/nate/Documents/haskell/ProblemSets/lists.hs:68:8
    • In the first argument of ‘len’, namely ‘y’
      In the second argument of ‘(+)’, namely ‘len y’
      In the expression: 1 + len y
    • Relevant bindings include
        y :: b
          (bound at /home/nate/Documents/haskell/ProblemSets/lists.hs:70:35)
        len :: Pair a b -> Integer
          (bound at /home/nate/Documents/haskell/ProblemSets/lists.hs:69:1)
Compilation failed.


I think the problem here is that len expects an argument of type (Pair a b), but cdr returns something of type b, and the compiler can't verify that b is always going to of type (Pair a0 b0).

The maddening thing is that the call "cdr (cdr l3)" works!  I'd think that the same trouble of verifying thet (cdr l3) returns a Pair type would apply in this situation as well, but apparently not.

I tried a couple different variations on this, including...
-}

data List a = Empty | Cons a (List a) | ListCons (List a) (List a)

{-

The problem here is that you would need to have a procedure

car:  List a -> a

to handle lists with an atom as the first element, another one

car:  List a -> List a

for the nested lists.


So, I'm trying to figure my way out of this so I can do recursion on cdr.  The next step I suppose would be to try some sort of tree like structure...

Any thoughts you have would be appreciated.  No rush.

-- Nate

-}

{-
len :: Pair a (Pair a b) -> Integer
len NullPair = 0
len p = 1 + len $ thisCdr p

lists.hs:117:19-27: error: …
    • Couldn't match expected type ‘Pair a0 b0’ with actual type ‘b’
      ‘b’ is a rigid type variable bound by
        the type signature for:
          len :: forall a b. Pair a b -> Integer
        at /home/nate/Documents/haskell/ProblemSets/lists.hs:115:8
    • In the second argument of ‘($)’, namely ‘thisCdr p’
      In the expression: 1 + len $ thisCdr p
      In an equation for ‘len’: len p = 1 + len $ thisCdr p
    • Relevant bindings include
        p :: Pair a b
          (bound at /home/nate/Documents/haskell/ProblemSets/lists.hs:117:5)
        len :: Pair a b -> Integer
          (bound at /home/nate/Documents/haskell/ProblemSets/lists.hs:116:1)
Compilation failed.
-}
