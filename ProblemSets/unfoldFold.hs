

{-# LANGUAGE TemplateHaskell #-}
import Data.List (unfoldr)
import Hugs.observe

-- INVERTING FOLDR WITH UNFOLDR

-- Here we implement a version of the Godel numbering function 
-- godelNum: N^m -> N
-- defined by
-- [a_1,a_2,...,a_n] -> (p_1^a_1) * (p_2*a_2) * ... * (p_m^a_m)
-- where p_j is the jth prime.

-- This is a bijection by the fundamental theorem of arithmetic.

integersFrom :: Integer -> [Integer]
integersFrom k = k : map (+1) (integersFrom k)

sieve (s:tream) = s : (sieve (filter (\x -> not (x `mod` s == 0)) tream))
primes = sieve $ integersFrom 2

nthPrime n = head $ drop (n-1) primes

godelNum :: [Integer] -> Integer
godelNum coords = foldr (\(a,b) x -> x*(a^b)) 1 zs
  where zs = zip (take l primes) coords
        l = length coords
-- e.g.
-- godelNum [2,2,2] gives
-- 900 = 2^2 * 3^2 * 5^2



godelNumInv n = unfoldr (\(count, dividend, pIndex) ->
                           if dividend == 1 then Nothing
                           else Just (count,
                                      (primeIndex dividend (nthPrime pIndex),
                                       dividend `div` (nthPrime pIndex)^count,
                                       pIndex + 1)))
                                      (primeIndex n 2, n `div` 2^(primeIndex n 2) , 1)

ginv n = unfoldr (\(c, rem, i) -> if rem < 2 then Nothing
                 else Just (c, (primeIndex n (nthPrime i), divideOut' (nthPrime i-1) rem, i+1))) (primeIndex n 2, n , 2)
-- remove all factors of p from n
divideOut' p n
  | n == 1 = 0
  | otherwise = n `div` pFactor
  where pFactor = p^(primeIndex n p)

divideOut n p
  | n == 1 = 0
  | n `mod` p /= 0 = n
  | otherwise = divideOut (n `div` p) p
  
primeIndex n p = aux n p 0
  where
    aux n p count
      | n == 0 = 0
      | n `mod` p /= 0 = count
      | otherwise = aux (n `div` p) p (count + 1)
      

-- Here is an example:

-- sumInts takes a list of sequential ints [1,2,...,n] and sums them.

-- undoSum calculates number of sequential ints that would add up
-- to a given sum, then recreates the sequence.

sumInts :: [Int] -> Int
sumInts = foldr (+) 0

-- we solve for n in sum = n*(n+1)/2 ...taking positive root, of course. 
undoSum :: Int -> [Int]
undoSum sum = let n = ((-1)+sqrt(1+8*fromIntegral(sum))) / 2 in
                unfoldr(\(x,k) -> if k == 0 then Nothing else Just (x,(x+1,k-1))) (1,n)

-- undoSum $ sumInts [1,2,3,4,5] gives  [1,2,3,4,5]
-- (But then so does undoSum $ sumInts [3,2,5,4,1])
-- and sumInts $ undoSum 15 gives 15

-- Here is a TANGENT:
-- There is no inverse to the collatz function:
collatz n = col n ++ [1]
col n  = unfoldr (\a -> if a == 1 then Nothing
                       else Just (a, nextTerm a)) n
           where nextTerm c
                   | odd c = (3*c + 1)
                   | otherwise = c `div` 2

-- That is, if I gave you a number that was part of some Collatz sequence, it would be impossible for you to determine which number originated the sequence.  For example, 16 has its origin in both 6 and 50:
-- [6,3,10,5,16,...]
-- [50,25,76,38,19,58,29,88,44,22,11,34,17,52,26,13,40,20,10,5,16,...]
-- However, it would be possible to form a binary tree that contained all possible solutions...
{-
                                          [16]
                                          /   \
                                     [5,16]   [32,16]
                                      /  \      /   \
                               [10,5,16]  []   []   [64,32,16]
                               /     \                 /     \  
                    [3,10,5,16]    [20,10,5,16] [21,64,32,16] [128,64,32,16]
-}
-- etc...
-- Knowing the length of the original series would give  O(2^length) elements in the inverse image of the
-- function.
-- All of which makes me wonder if there is something like a treeUnfoldr function with conditional branching.
-- Like...
-- treeUnfoldr <b1> <b2> <start_value>
-- treeUnfoldr <if x mod 3 == 1, new node>  <always make node 2*x>

-- END TANGENT




