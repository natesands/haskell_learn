-- Nate Sands
-- 30 March 2020
-- CSc510003
-- Prof. Troeger
-- CCNY

-- PROBLEM SET 5 --


{-# LANGUAGE ParallelListComp #-}

import Data.Ratio

--------------------------------

natNums :: [Integer]
natNums = 0 : map (+1) natNums

integersFrom :: Integer -> [Integer]
integersFrom k = k : map (+1) (integersFrom k)

---------------------------------

{-
PROBLEM 1
Construct the list of prime numbers, using the method known as the sieve of Eratosthenes (see
  Abelson and Sussman, Section 3.5.2)
-}

sieve (s:tream) = s : (sieve (filter (\x -> not (x `mod` s == 0)) tream))
primes = sieve $ integersFrom 2

{-
PROBLEM 2
  Find the stream-based definition of the predicate prime? given in Abelson and Sussman, and 
  implement it in Haskell. Prove that your function works.

  I note that the question mark seems to cause problems, and that a quick look on the web has not
  cleared this up for me.  From the Haskell Report, it appears that ? is valid syntax, but perhaps
  an operator rather than a character which can occur in identifier names.  Request: figure this 
  out!  Suggestion for now: call your predicate prime, rather than prime?

-}

prime n = iter primes
  where iter ps
          | (head ps)^2 > n = True
          | n `mod` (head ps) == 0 = False
          | otherwise = iter $ tail ps


-- Proof --
-- We assume n an integer, n >= 2.

-- Invariant: The prime factorization of n does not contain any prime less than
-- head ps. This is trivially the case when the initial call to
-- iter primes is made. If (head ps)^2 < n and (head ps) does not divide  n,
-- (i.e. the first two conditions in the body of iter are not met)
-- then the 'otherwise' clause shifts (head ps) to the next prime in the list,
-- maintaining the invariant.

-- We note that an integer n is composite if and only if it has at least one prime factor p
-- such that p <= sqrt(n) (or equivalently p^2 <= n).  Thus if p^2 > n for some prime p, then
-- either n has some smaller prime factor, or n is itself a prime.

-- If n = 2, then (head ps) = 2, and 2^2 > 2, and the procedure returns true.

-- For n > 2, if at any iteration (head ps)^2 > n, then by the invariant n has no prime
-- factor smaller than (head ps).  Thus n must be prime by the note above.  The procedure correctly
-- returns true in this case.  If (head ps) divides n, then n must composite**, and the procedure
-- returns false.

-- **(Other than for n = 2, it cannot ever be the case that (head ps) = n.  If n is prime, there
-- always exists a minimal prime p, 2 <= p < n, such that n < p^2.  This guarantees that the
-- procedure will terminate as soon as (head ps) = p < n for n prime.)


{-
PROBLEM 3
  Write a function addLists, which produces the list of element-wise sums of two non-empty 
  lists of numbers. Write this function in a couple of ways -- one using zip, and others not using zip.
-}

addLists :: (Num a) => [a] -> [a] -> [a]
addLists xs ys = zipWith (+) xs ys

addLists' :: (Num a) => [a] -> [a] -> [a]
addLists' _ [] = []
addLists' [] _ = []
addLists' (x:xs) (y:ys) = (x + y) : (addLists' xs ys)


{-
  use your function to give another definition of the list of integers 
  (ie, integersFrom 1), defined above, as well as yet another definition of the list of Fibonacci
  numbers.  
-}

ones :: [Integer]
ones = 1 : ones

integersFrom' :: Integer -> [Integer]
integersFrom' n = n : addLists ones (integersFrom' n)

fibs :: [Integer]
fibs = 0 : 1 : addLists fibs (tail fibs)

{-
  Then generalize these to define a couple of functions mapLists which input a binary function f and two
  lists and which returns the element-wise result of applying f to corresponding elements in both lists. 

-}

mapLists f xs ys = [ f a b | (a,b) <- zip xs ys ]

mapLists' f [] _ = []
mapLists' f _ [] = []
mapLists' f (x:xs) (y:ys) = (f x y) : mapLists' f xs ys


{-
PROBLEM 4

 Solve (in Haskell!) Exercise 3.56 from Abelson and Sussman.  Explain your code.

-}

-- Merge function, where s1 and s2 are streams 
-- (using case construction to make the function
-- as close to Scheme version in A.S.).
-- Performs similar function to merge in mergesort
-- except that if n = (head s1) = (head s2), only one copy
-- of n is transferred to merged list.
merge s1 s2
  | s1 == [] = s2
  | s2 == [] = s1
  | otherwise =
    let s1head = head s1;
        s2head = head s2
    in
    case True of
      True | s1head < s2head ->
             s1head : merge (tail s1) s2
           | s1head > s2head ->
             s2head : merge  s1 (tail s2)
           | otherwise ->
             s1head : merge (tail s1) (tail s2)

hammingS = 1: merge ( merge s_2 s_3) s_5
  where s_2 = map (*2) hammingS
        s_3 = map (*3) hammingS
        s_5 = map (*5) hammingS

-- PROOF:
-- We need to show that hammingS contains only numbers
-- whose factorization includes only powers of 2, 3,  and 5, 
-- and that it contains every such number.

-- Base case: The first element is 1, which is 2^0*3^0*5^0.

-- Assume that the nth element of hammingS is the product
-- of powers of 2,3, and 5 for 1 < n <= k.  The entries
-- of s_1,s_2, and s_5 are calculated by multiplying these
-- elements by 3, 2, or 5.  The merge function chooses
-- element k+1 from these three list, so it must be
-- a some power of 2,3, and 5.

-- To see that every possible product of powers of 2, 3, and
-- 5 are produced, we note that because 1 is an element of
-- hammingS, so is 2*1, 3*1, 5*1.  And since these are
-- elements, 2*2, 2*3, 2*5, 3*3, 3*5 are also elements, and so
-- on, as merge progressively adds terms by multiplying previous
-- terms by 2, 3, and 5.  Thus every product of powers of
-- 2, 3, and 5 is eventually merged into the list.   
--------------------------------------------------------------------------------
-- From previous draft:
-- To get a sense of how this code works, let us look at the simpler case of the function:

h = 1 : merge s_2 s_3
  where s_2 = map (*2) h
        s_3 = map (*3) h

{-
We have that h = [h1, h2, h3, ...]
Each calculation adds another number to the list, which allows
subsequent calculations to be made:
h = 
1 : merge [2*h1, 2*h2, 2*h3, ...] [3*h1, 3*h2, 3*h3, ...] =
1 : merge [2, 2*h2, 2*h3, ... ] [3, 3*h2, 3*h3, ... ] =
1 : 2 : merge [2*h2, 2*h3, ...] [3, 3*h2, 3*h3, ... ] =
1 : 2 : merge [4, 2*h3, ...] [3, 6, 3*h3, ... ] =
1 : 2 : 3 : merge [4, 2*h3, ... ] [6, 3*h3, ...] =
1 : 2 : 3 : 4 : merge [2*h3, ...] [6, 3*h3, ...] =
1 : 2 : 3 : 4 : merge [6, ...] [6, 9, ...] =
1 : 2 : 3 : 4 : 6 : merge [...] [9, ...] =
1 : 2 : 3 : 4 : 6 : ...

In the full case including s_5, we have... 
1 : (merge [2*h1, 2*h2, 2*h3, ..] [3*h1, 3*h2, 3*h3,... ]) `merge` [5*h1, 5*h2, 5*h3...]
1 : (merge [2, 2*h2, 2*h3, ..] [3, 3*h2, 3*h3,... ]) `merge` [5, 5*h2, 5*h3...]
1 : 2 : (merge [2*h2, 2*h3, ..] [3, 3*h2, 3*h3, ... ]) `merge' [5, 5*h2, 5*h3, ...]
1 : 2 : (merge [4, 2*h3, ... ] [3, 6, 3*h3, ... ]) `merge' [5, 10, 5*h3, ...]
1 : 2 : 3 : (merge [4, 6, ...] [6, 9, ... ]) `merge` [5, 10, 15, ...]
1 : 2 : 3 : 4 : (merge [6,..] [6, 9, ... ]) `merge` [5, 10, 15,...]
1 : 2 : 3 : 4 : 5 : 6 : 9: 10 : .. etc

-}

{-
PROBLEM 5
  Represent the harmonic series as a list -- as part of our exploration of the type hierarchy, you should 
  experiment with the type Rational, after 

    import Data.Ratio

  somewhere near the top of your file.  The goal is to have a list of exact fractions, rather than
  floating point approximations. 
-}
harmonicTerms :: [Rational] 
harmonicTerms = map (\x -> 1 % x) (integersFrom 1) -- [1/1, 1/2, 1/3, ... ]
harmonicSeries :: [Rational]
harmonicSeries = scanl1 (+) harmonicTerms -- partial sums  S_n =  (1/1 + 1/2 + ... + 1/n), [S_1, S_2, ... ]

-- sample output
-- take 10 harmonicSeries :
-- [1 % 1,3 % 2,11 % 6,25 % 12,137 % 60,49 %

{-
PROBLEM 6
  Solve (in Haskell) Exercise 3.59(a) from Abelson and Sussman.  Two solutions are wanted --
    
   integrateSeries :: [Double] -> [Double]
  and
   integrateSeries' :: [Double] -> [Rational]
-}

-- input is stream of coefficients: a0,a1,a2...
-- output is a0,(1/2)*a1, (1/3)*a2,...
integrateSeries :: [Double] -> [Double]
integrateSeries stream = zipWith (*) stream (reciprocal (integersFrom 1))
  where
    reciprocal :: (Integral a, Fractional b) => [a] -> [b]
    reciprocal xs = map (\x -> (1/) $ fromIntegral x) xs


integrateSeries' stream = zipWith (*) stream (reciprocal' (integersFrom 1))
  where
    reciprocal' xs = map (\x -> (1 % (fromIntegral x))) xs

{-
PROBLEM 7
  Solve (in Haskell) Exercise 3.59(b) from Abelson and Sussman.  Your solutions should
  show exact (that is, rational) coefficients.  
-}
-- generate power series for exp, sin, cos
expSeries = 1 : integrateSeries' expSeries

cosineSeries = 1 : map (*(-1)) (integrateSeries' sineSeries)
sineSeries = 0 : integrateSeries' cosineSeries

{-
Problem 8
  Solve (in Haskell) Exercise 3.60 from Abelson and Sussman. 
-}

-- Procedure for multiplying series:
-- We write the power series sum(a_i*x^i) as [a0,a1,a2,...].
-- Then the product of two power series [a0,a1,a2,...] * [b0,b1,b2,...]
-- is equal to:
--     [a0*b0, a0*b1, a0*b2, a0*b3, ... ]
--   + [    0, a1*b0, a1*b1, a1*b2, ... ]
--   + [    0,     0, a2*b0, a2*b1, ... ]
--   + ...
mulSeries (x:xs) (y:ys) = x*y : addLists (map (* x) (ys)) (mulSeries (xs) (y:ys))

-- test using
--     take 5 $ addLists (mulSeries sineSeries sineSeries) (mulSeries cosineSeries cosineSeries)
-- gives
--     [1 % 1,0 % 1,0 % 1,0 % 1,0 % 1]
-- and summing all the terms gives 1.

{-
PROBLEM 9
  Write a program powerTable which returns an infinite list whose first element is the list of all squares of positive integers,
  whose second element is the list of all cubes of positive integers, and so on.
-}

mapPower :: Integer -> [Integer] -> [Integer]
mapPower n xs = map (\x -> x^n) xs

powerTable = [ mapPower n ints | n <- (integersFrom 2) ]
  where ints = integersFrom 1

{-
λ> take 5 $ (powerTable !! 0)
[1,4,9,16,25]
λ> take 5 $ (powerTable !! 1)
[1,8,27,64,125]
λ> take 5 $ (powerTable !! 2)
[1,16,81,256,625]
-}
