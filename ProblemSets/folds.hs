{-

foldr :: (a -> b -> b) -> t a -> b
foldr f z t = appEndo (foldMap (End #. f) t ) z

t has kind * -> *

newtype is like data but it has exactly one constructor
and exactly one field

e.g.

newtype State s a = State { runState :: s -> (s,a) }

a 'semigroup' is set with an associative binary operation
(e.g. the set of all strings on 26 letters with the operation of concatenation)

a 'monoid' is an algebraic structure with an associative binary operation and an identity element
(e.g. all functions from a set to itself under function composition)

the "free monoid" on a set is the monoid whose elements are all the finite sequences (or strings) of zero or more elements from that set, with string concatenation as the monoid operation and with the unique sequence of zero elements, often call the empty string, denoted \epsilon or \lambda as the identity element.  The free monoid on A is denoted A*

a monoid homomorphism corresponds to a map operation applying f to all elements of a list followed by a fold which combines the results using the binary operator *.  
