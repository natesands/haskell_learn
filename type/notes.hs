{-

Haskell has a static type system.  Every expression is categorized according to its type, which is known at compile time.


TYPES AND TYPECLASSES
---------------------

Types:
  Int
  Integer (same as Int but unbounded; needed for infinite lists?)
  Float
  Double
  Bool
  Char

Type Classes:
  Eq  -- tests for equality (=, \=)
  Ord -- orderings (<=, >=, <, >)
  Show -- print argument as String (show)
  Read -- turn String into other type (read)
  Enum -- perform sequential functions (succ, pred)
  Bounded -- gives maxBound and minBound for all types
  Num -- numeric operations for Int, Integer, Float, Double
    class (Eq a) => Num a
  Integral -- sub-Type class of Num, handles only Int and Integer
  Floating -- sub-Type class of Num, handles only Float and Double
  Functor -- "things that can be mapped over"
    class Functor f where
      fmap :: (a -> b) -> f a -> f b

  
???
  Foldable (Data structures that can be folded)

Typeclasses are interfaces which provide specific functionality.


The type signature of foldr is
foldr:: Foldable t => (a -> b -> b) -> b -> t a -> b

Here 't a' says that this parameter must be a foldable object
containing elements of type a.

In order to accomodate infinite list of integers, e.g. primes,
these lists must be of type [Integer] instead of [Int] (True?)

To cast an Int to an Integer, use toInteger

-}
