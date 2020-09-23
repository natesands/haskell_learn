import qualified Data.Monoid as M
import qualified Data.Foldable as F
-- The Foldable type class contains foldMap function which has type


data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq, Read)
{-
-- foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m

instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Node x l r) = foldMap f l `mappend`
                           f x `mappend`
                           foldMap f r
-}

-- our function f takes a tree value and reduces to monoid value,
-- then joins the result using the monoid mappend function


tt1 = Node 5
           ( Node 3
             (Node 1 Empty Empty)
             (Node 6 Empty Empty)
           )
           (Node 9
             (Node 8 Empty Empty)
             (Node 10 Empty Empty)
           )


tt2 = Node 5
            ( Node 3 Empty Empty)
            ( Node 9 Empty Empty)

tt3 = Node 5 (Node 4 Empty Empty) Empty
tt4 = Node 5 Empty (Node 4 Empty Empty)
tt5 = Node 25 (Node 8 Empty Empty) (Node 3 Empty Empty)

-- x-minus-y function to test folds
xmy = (\x y -> x-y)
{-
Note: defining foldMap corresponds the way we have corresponds to an in order tree traversal.

λ> foldr xmy 0 tt5
-14
λ> foldr xmy 0 [8, 25, 3]
-14
--(3 - (25 - (8 - 0))

λ> foldl xmy 0 tt5
-36
λ> foldl xmy 0 [8, 25, 3]
-36
(((0 - 25) - 8) - 3)

If instead we defined foldMap according to a pre-order traversal:

instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Node x l r) = f x `mappend`
                           foldMap f l `mappend`
                           foldMap f r

then we would have

λ> foldr xmy 0 tt5
20
λ> foldr xmy 0 [25,8,3]
20
λ> foldl xmy 0 tt5
-36
λ> foldl xmy 0 [25,8,3]
-36

-}



inOrder :: Tree a -> [a]
inOrder Empty = []
inOrder (Node x l r) = (inOrder l) ++ [x] ++ (inOrder r)

preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node x l r) = [x] ++ (preOrder l) ++ (preOrder r)

postOrder :: Tree a -> [a]
postOrder Empty = []
postOrder (Node x l r) = (postOrder l) ++ (postOrder r) ++ [x]


{-
λ> foldr
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b

f x1 (f x2 (f x3 (... (f xn s))))

λ> foldr (:) [] tt1
[1,3,6,5,8,9,10]
-- Corresponds to in order tree traversal 
λ> foldl
foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b



QUESTION:  how do foldl and foldr make calls to foldMap??  In the above example, the function xmy = (\x y-> x -y) returns an integer value, which does not have an instance as a monoid.. unless they are, say, wrapped in Product or Sum.
-}



{-
Why are there right-folds and left-folds?

In the case of associative functions like (+), left-association is equal to right-association, so foldl and foldr are the same.

foldr can work with infinite lists, whereas foldl cannot ...
e.g.

myAny :: (a -> Bool) -> [a] -> Bool
myAny p list = foldr step False list
  where
    step item acc = p item || acc

>> myAny even [1..]
see "Why does this Haskell code work successfully with infinite lists?"

for foldr, the recursive call is imbedded in the parameter of the function f.
If a function has strict evaluation on the first parameter then it will terminate given the right condition.  
-}

{-
def of foldr

foldr :: (a -> b -> b) -> b -> t a -> b
foldr f z t = appEndo (foldMap (Endo #. f) t) z

-}

{-
In category theory a functor is a mapping between objects in one category to another, satisfying

F(id_A) = id_B
F(f.g) = F(f).F(g)

The functor class is for types that can be mapped over...

If a f is an instance of Functor, then it implements

fmap :: (a -> b) -> f b -> f a

e.g. [] is a type constructor and an instance of functor


fmap id = id // where the identity is different on both sides
fmap (f . g)

mconcat :: Monoid m => [m] -> m
mconcat = foldr mappend mempty

e.g. mconcat [[1,2], [3,4]] = [1,2,3,4]

so mconcat folds a list of Monoid instances into a monoid, but we want to be able to fold a list with elements of any type.

we can do this using foldMap :: (Monoid m, Foldable t) => (a->m) -> t a -> m
(recall foldMap is defined under Foldable typeclass)

which will convert elements to some Monoid type

mconcat :: Monoid m => [m] -> m
fmap :: (a -> b) -> f a-> f b
g :: (a -> m)
foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
foldMap g = mconcat . fmap g

if g has type :
Monoid m => (a -> m)

fmap g has type:
Monoid m => f a -> f m

So g takes a type a and returns a monoid.
fmap g takes a Foldable concrete type a and turns it into a foldable concrete of monoid type

mconcat . fmap g
takes a Foldable concrete type a and turns it into a foldable concrete of monoid type and then returns a monoid type
fmap takes g and maps it over a functor f a, giving a functor f m , mconcat folds this in to a monoid.  

[a] -> [

-}
newtype Sum a = Sum {getSum :: a} deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  mappend (Sum x) (Sum y) = Sum (x+y)
  mconcat = foldr mappend mempty
{-
λ> map Sum [1,2,3,4,5]
[Sum {getSum = 1},Sum {getSum = 2},Sum {getSum = 3},Sum {getSum = 4},Sum {getSum = 5}]
λ> mconcat $ map Sum [1,2,3,4,5]
Sum {getSum = 15}
-}

{- so now Sum :: (Num a, Monoid m) => a -> m

fmap .  Sum :: -}

{-
mappings a -> a, i.e. endomorphisms, form a monoid under function composition.  Suppose f: a -> a, g: a -> a

mempty = id
mappend f g = f . g
mconcat = foldr mappend mempty

mconcat [f,g,h] = f `mappend` ( g `mappend` (h `mappend` id))
                = f . g . h . id

we require

f . id = f
id . f = f
-}

