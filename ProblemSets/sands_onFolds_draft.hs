-- Nate Sands
-- 19 May 2020
-- CSc510003
-- Prof. Troeger
-- CCNY



import qualified Data.Monoid as M
import qualified Data.Foldable as F

-- On the Definition and Context of Folds in Haskell
-- DRAFT

{-

INTRO
-----

Here are some prototypical examples of folds:

foldr (++) [] ["h","e","l","l","o"] = "hello"

foldr (+) 0 [1,2,3,4,5] = 15

foldr (*) 1 [1,2,3,4,5] = 120


In each case, we are collapsing a set of values, including an identity element ([], 0, 1), into a single value by repeatedly appying an associative binary function ('++','+','*').  This is an elementary operation that can be implemented easily in any number of languages.  But Haskell gives the fold a larger semantic value within the typeclass system.

Sets that are closed under an associative binary operation and that contain an identity element are called 'monoids' in abstract algebra.  Haskell provides a Monoid typeclass that defines types containg both a binary operation and an identity.  But Monoids also have the additional property that they can be folded.  Even though there is separate Foldable typeclass in Haskell, properly speaking it is the Monoid typeclass that defines what it means to be foldable. 


THE ALGEBRAIC STRUCTURE OF TYPECLASSES
--------------------------------------

In the Haskell documentation, typeclasses often come with a set of "laws" that all instances are expected to obey. Sometimes these are stated explicitly, other times they are left to be inferred from the class definition itself.  As an example, take the Magma typeclass (Data.Magma):

-}

class Magma a where
  (<>) :: a -> a -> a


{-
Here the only specification is that an instance of Magma define a binary operation (<>).  There is no other constraint on this function to be commutative, associative, or otherwise. Nor is there an expectation that the instance contain an identity element. This tracks closely with the algebraic definition of a magma.

Rock, Paper, Scissors is a commutative, non-associative magma without an identity element.
-}

data RPS = Rock | Paper | Scissors deriving (Show)

instance Magma RPS where
  Rock <> Paper = Paper
  Rock <> _  = Rock
  Paper <> Scissors = Scissors
  Paper <> _ = Paper
  Scissors <> Rock = Rock
  Scissors <> _ = Scissors

{-
λ> Rock <> Paper
Paper
λ> (Rock <> Paper) <> Scissors
Scissors
λ> Rock <> (Paper <> Scissors)
Rock


The definition of Semigroup is identical to Magma, except the Prelude documentation says:


class Semigroup a where
  (<>) :: a -> a -> a

Instances should satisfy the associativity law:
  x <> (y <> z) = (x <> y) <> z


These "should sastify" laws are not enforced.  Instead they set specifications for the language which, when observed, create programs that can be reasoned about mathematically using principles of abstract algebra.  It is within the superclass of Semigroups that Monoid is defined.  

-- MONOIDS, FOLDS, and the FOLDABLE TYPECLASS

The Monoid typeclass adds an identity element (mempty) and a special function (mconcat) to the Semigroup superclass:

class Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mconcat :: [a] -> a

(Here mappend is synonymous with Semigroup's (<>))

According to the notes in Data.Monoid, a "minimal complete definition" of a Monoid requires that implementations of mempty and mappend be written explicitly, and any instance should satisfy the following laws:

1) mappend should be associative,
2) mempty should be a left and right inverse under mappend, and
3) mconcat must be equivalent to
   mconcat = foldr mappend mempty.

If we don't specify mconcat, does Monoid supply  a default implementation?
Apparently so.  For example...
-}

data Klein4 = A | B | C | E deriving (Show)

instance Monoid Klein4 where
  mempty = E
  mappend A B = C
  mappend B A = C
  mappend A C = B
  mappend C A = B
  mappend B C = A
  mappend C B = A
  mappend E (x) = x
  mappend (x) E = x
  mappend _ _ = E
  
{-
λ> mconcat [B,C,E,A,A]
A

Which, as promised, is equivalent to

λ> foldr (mappend) E [B,C,E,A,A]
A

***

Suppose that I create a new type that I want to be able to fold, so I declare it as an instance of the Foldable typeclass.  The class Pragma declares a minimal defintion of either foldr or foldMap, which is why these functions are defined in terms of each other:

foldMap :: Monoid m => (a -> m) -> t a -> m
foldMap f = foldr (mappend . f) mempty

and

foldr :: (a -> b -> b) -> b -> t a -> b
foldr f z t = appEndo (foldMap (Endo . f) t) z

If one is written, then the other is filled in, as is foldl:

foldl :: (b -> a -> b) -> b -> t a -> b
foldl f z t = appendo (getDual (foldMap (Dual . Endo . flip f) t)) z

This is the example given in the class notes for an implementation of foldMap:
-}

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq, Read)

instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Node x l r) = foldMap f l `mappend`
                           f x `mappend`
                           foldMap f r
{-
The order of operations corresponds to an in-order tree traversal.

So if we have a Klein4 Tree and use foldMap with an identity function, we get the same as if we had applied mconcat to an in-order list:
-}
-- in-order = [B,C,E,A,A], same as in the earlier example.
ttK4 = Node A (Node C (Node B Empty Empty) (Node E Empty Empty)) (Node A Empty Empty)

{-

λ> foldMap (\x -> x) ttK4
A

The interpreter has no problem with this, because it recognizes the Tree as containing monoid values, so it can use the defined values for mempty and mappend.

But now suppose we try to use this implementation on tree of type Tree Int:

-}

ttInt= Node 25 (Node 8 Empty Empty) (Node 3 Empty Empty)


{-

We get an error!  Because there is no instance of (Monoid Int) defined that would tell the interpreter what 'mempty' and 'mappend' mean in this context.

Nonetheless, both foldr and foldl produce expected results when we include a function and a z value:


λ> foldr (-) 0 ttInt
-14
   = 8 - (25 - (3 - 0))

λ> foldl (-) 0 ttInt
-36
   = ((0 - 8) - 25) -3

Why is this the case?  Let's look at the definition of foldr again:

foldr f z t = appEndo (foldMap (Endo . f) t) z

Unlike before foldMap is able to deduce a monoid instance, but this time it is in the form (Endo . f).  


-- ENDO

Data.Monoid defines an endomorphism type "Endo a":

   newtype Endo a = Endo { appEndo :: a -> a } deriving (Generic)

Endo has a single field containing an function (a -> a).  It is declared as an instance of the Monoid class within Data.Monoid:

instance Monoid (Endo a) where
  mempty = id
  mappend (Endo f) (Endo g) = Endo (f . g)

Here the identity is the identity function, and the binary operation is function composition.  (Endomorphisms form a monoid under function composition, but strinctly speaking, we should expect that any such f ∈ Monoid a => (Endo a) would obey
   f (idₐ) = idₐ
   f (x <> y) = f (x) <> f (y)
Although at the moment I don't see any reference to this requirement in the documentation.)

As before, letting

instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Node x l r) = foldMap f l `mappend`
                           f x `mappend`
                           foldMap f r

ttInt= Node 25 (Node 8 Empty Empty) (Node 3 Empty Empty)

then...

foldr (-) 0 ttInt = appEndo (foldMap (Endo . (-) ) ttInt) 0
                  = appEndo (foldMap (Endo . (-) ) (Node 8 Empty Empty)  <>
                            (Endo . (-) )  25  <>
                            (foldMap (Endo . (-) ) (Node 3 Empty Empty)) 0

                  = appEndo ((foldMap (Endo . (-) ) Empty <> (Endo . (-) ) 8 <> (foldMap (Endo . (-) ) Empty)) <>
                            (Endo (-) 25)) <>
                            ((foldMap (Endo . (-) ) Empty <> (Endo . (-) ) 3 <> (foldMap (Endo . (-)) Empty))) 0
                  = appEndo ( id <> (Endo (-) 8) <> id <>
                             (Endo (-) 25) <>
                             id <> (Endo (-) 3) <> id ) 0
                  = appEndo ( Endo (id . (-) 8 . id . (-) 25 . id . (-) 3 . id)) 0
                  = (id . (-) 8 . id . (-) 25 . id . (-3) . id) 0
                  = (-) 8 . (-) 25 . (-) 3 0
                  = (8 - ( 25 - (3 - 0)))
                  = -14

So by using (End . f) as the argument to foldMap, we are able to turn the elements of the Tree into curried functions, and then compose them.


                  

NOTES/QUESTIONS
--
What is the function of Dual?

What are the fold laws? (See Bird's books)

How does Categorical Logic account for the structure of Haskell's type system?

what is the :| operator?

What is the MapReduce paradigm?


in prelude notes on foldr

"For a general Foldable structure this should be semantically identical to

foldr f z = foldr f z . toList"


fmap is properly an endofunctor in the category of types....

-}
