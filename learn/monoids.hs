import Data.Monoid

{-

Algebraic definition of a monoid is a set together a binary associative operation and an identity element

class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty

only concrete types m can be monoids

mempty represents the identity value for a monoid, which must
be a left and a right identity

mappend is the binary associative function

mconcat takes a list of monoid values and reduces them
to a single value by applying the binary operation.

this is implem ented through the use of a fold

e.g.

instance Monoid [a] where
  mempty = []
  mappend = (++)

(by not defining mconcat we go with the default implementation)

Monoid laws:

mempty `mappend` x = x
x `mappend` mempty = x

(x `mappend` y) `mappend` z = x `mappend` z = x `mappend` (y `mappend` z)


NOTE: Commutivity is not required!!

mappend "two" "three" != mappend "three" "two"

Data.Monoid exports two modules, Product and Sum

newtype Product a = Product { getProduct = a }
  deriving (Eq, Ord, Read, Show, Bounded)

Product's instance for Monoid is

instance Num a => Monoid (Product a) where
  mempty = Product 1
  Product x `mappend` Product y = Product (x * y)

λ> mconcat :: [a]

-}

