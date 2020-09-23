







-- ON FOLDS

-- INTRO
{-
One one the most striking features of Haskell is the way it attempts to encapsulate fundamental operations --basic arithmetic calculations, mapping and folding functions -- in a more general algebraic framework.  This is accomplished using the typeclass system.  Our text compares typeclasses to interfaces in Java in that they define a set of functions that are inherited by instances of the typeclass.  But Haskell adds an explicit layer of organization around concepts such as 'monoids', 'functors', 'semigroups'. In doing so it seems that the intent of the creators was to build in a high-level of logical structure that can be used to reason  programs.

-}

-- MONOIDS

{-
The algebraic definition of a 'monoid' is a set M together with a  binary operation '*' and an identity element 'e' satisfying:
  a * e = e * a = a
  (a * b) * c = a * (b * c)
for a, b, c in M.

If we let M be the set of all strings over some alphabet, and consider concatenation as a binary operation, then M meets the conditions of a monoid.

Haskell embeds this structure in a literal way with the application of the (++) operator over character strings, and, more generally to lists of any kind.  If we take M to be the set of lists of type [a], then
    * = (++)
    e = []
and clearly  l1 ++ (l2 ++ l3) = (l1 ++ l2) ++ l3.  Hence, the elementary operation of joining lists together can be considered as more than just a 'feature' of the language, but as particular instance of a more general concept.

Haskell formalizes the structure of a monoid in its Monoid typeclass.

class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty

Any instance of the Monoid typeclass is expected to define an identity element mempty, and an associative binary operation mappend.  Where Haskell's conception diverges slightly from the algebraic notion of a monoid, is the addition of a the mconcat operation, which, generically, takes a list of elements of monoid type and reduces them to a single monoid value.
-}

-- FUNCTORS

{-
Haskell's map function is defined simply:

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

Observe that the first argument to map is a function f: a->b.  map transforms f into another function (map f): [a] -> [b].  We also have that

  map id = id     (1)

where the identity on the left hand side is id: a->a, and the identity on the right is id: [a] -> [a].  Also, for functions f: a->c, g: c->b,

  map (g . f) = map g . map f   (2)

A 'map of maps' that satisfies properties (1) and (2) is called a functor in category theory, i.e., for F a functor,

F(id_A) = id_B , and
F(f . g) = F(f).F(g)

Here again, Haskell embodies a general algebraic notion in a type class:

class Functor f where
  fmap :: (a -> b) -> f a -> f b

If we look at the type signature, we see that fmap takes a function as its first parameter, and a concrete type (f a), where f is a type constructor, as its second parameter.  The return value is of type (f b). There is an 'expectation' that any type that is an instance of Functor will implement fmap in such a way that it satisfies the two functor laws above (although it is not clear to me at the moment how this is enforced within the language).

The list type constructor '[]' is just an instance of the Functor type class where fmap = map. Any type constructor (Maybe, Tree) which allows for a legal implementation of fmap may be an instance of the Functor class, which is why Functor class members are described as being 'things that can be mapped over'.
-}

-- FOLDS

{-

Foldable type 
foldMap takes a foldable structure containg values of type a, maps a function over these values that takes a to a monoid value, then joins them into a single monoid value.


Haskell's construction of the Monoid typeclass includes the function 'mconcat' which generalizes the idea of taking a list of monoidal values 




and
  map (f . g) = map f . map g 


homomorphism of monoids.

Questions to answer:
why do we need right folds and left folds?  
