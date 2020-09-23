{-# LANGUAGE ParallelListComp #-}
factorial n = product [1..n]
factorial2 n = product (enumFromTo 1 n)
-- point-free style
factorial3  = product . enumFromTo 1

-- in one line

result = let { factorial n | n > 0 = n * factorial (n-1); factorial _ = 1} in
  factorial 5

result2 = factorial 5 where factorial = product . enumFromTo 1

-- RVPN calculator
-- note:  words is a function String -> [String]
calc :: String -> [Float]
calc = foldl f [] . words
  where
    f (x:y:zs) "+" = (y+x):zs
    f (x:y:zs) "-" = (y-x):zs
    f (x:y:zs) "*" = (y * x):zs
    f (x:y:zs) "FLIP" = y:x:zs
    f xs y = read y : xs

-- here is the def of foldl
-- notice that f must be a function of
-- two vrariables

foldl' :: (a -> a -> a) -> a -> [a] -> a
foldl' f z [] = z
foldl' f z (x:xs) =foldl' f (f z x) xs
    
{- so run calc "1 2 +"
calc "1 2 +"
foldl f [] ["1","2","+"]
foldl f (read "1" : []) ["2", "+"]
foldl f [1] ["2," "+"]
foldl f [2,1] ["+"]
foldl (2+1):[] []
foldl [3] []
= [3]
-}

-- linear time fibonacci
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

{-
This is an example of corecursion...
0 : 1 : [ 0+1, 1+(0+1), (0+1) + (1 +(0 + 1)), ...]
0:1:[1, 2, 3, 5...]
-}



fibs2 = 0 : 1 : [ a + b | a <- fibs2 | b <- tail fibs ]

-- this is equivalent to

fibs3 = 0 : 1 : (zipWith (+) [ a | a <- fibs3] [ b | b <- tail fibs3 ])
