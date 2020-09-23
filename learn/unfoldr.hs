{-# LANGUAGE TemplateHaskell #-}
import Data.List (unfoldr)


{- maybe takes a default value, a function and a Maybe value. If the
the Maybe value is Nothing it returns the default value, otherwise
it applies the function to the maybe value -}

{-

foldr1 :: Foldable t => (a -> a -> a) -> t a -> a
foldr1 f [x] = x
foldr1 f (x:xs) = f x

foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
foldr f s [] = s
foldr f s (x:xs) = f x (foldr f s xs)


-}


pow2 :: [Integer]
pow2 = unfoldr (\x -> Just (x, x*2)) (1)

fibs :: [Integer]
fibs = unfoldr(\(a,b) -> Just (a,(b,a+b))) (0,1)


-- When we speak of inverting a function, what do we mean?  We are given a function f, and we need a function g such that f . g = g . f = id


-- collatz is not invertible because it is not injective.
-- 5 --> 16, but 32 --> 16

listOfNums n = unfoldr (\(a,b)-> if a == b + 1 then Nothing else Just (a, (a+1,b))) (1,n)

t1 init = unfoldr(\(a,b)-> if b == 0 then Nothing else Just (a, (a+1,b-a))) (1,init)

s1 lstOfNums = foldr (+) 0 lstOfNums

tt1 n = t1 (n * (n + 1) `div` 2)

sumOfInts n = foldr (+) 0 (listOfNums n)

undoSum :: Int -> [Int]
undoSum sum = let n = ((-1)+sqrt(1+8*fromIntegral(sum))) / 2 in
                unfoldr(\(x,k) -> if k == 0 then Nothing else Just (x,(x+1,k-1))) (1,n)
sumInts :: [Int] -> Int
sumInts = foldr (+) 0


collatz n = col n ++ [1]
col n  = unfoldr (\a -> if a == 1 then Nothing
                       else Just (a, nextTerm a)) n
           where nextTerm c
                   | odd c = (3*c + 1)
                   | otherwise = c `div` 2

