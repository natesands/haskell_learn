-- curried functions and partial application

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

multTwoWithNine :: (Num a) => a -> a -> a
multTwoWithNine y z = multThree 9 y z

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred = compare 100

-- infix functions

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

isUpperAlphanum :: Char -> Bool
isUpperAlphanum = (`elem` ['A'..'Z'])

-- NOTE HOWEVER (-4) means minus 4, not subtract 4.
-- to get (x - 4) use (subtract 4) x

-- Higher Order functions
multByTwo :: Int -> Int
multByTwo = (* 2)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

normalZip :: [a] -> [b] -> [(a,b)]
normalZip _ [] = []
normalZip [] _ = []
normalZip (x:xs) (y:ys) = (x, y) : normalZip xs ys

flip' :: (x -> y -> z) -> ( y -> x -> z)
flip' f = g
  where g x y = f y x
--or

flip2 :: (x -> y -> z) -> (y -> x -> z)
flip2 f x y = f y x
  
-- MAPS & FILTERS

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs

-- note: a 'predicate' is a function that returns a boolean value
-- filter applies a predicate to a list and then returns the
-- list of elements that satisfy the predicate

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' p (x:xs)
  | p x = x : filter' p xs
  | otherwise = filter' p xs

-- quicksort using filter

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort (filter (<= x) xs)
      biggerSorted = quicksort (filter (>x) xs)
  in smallerSorted ++ [x] ++ biggerSorted

-- find largest number under 100,000 divisible by 3829
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
  where p x = x `mod` 3829 == 0

-- Collatz sequences

chain :: (Integral a) => a -> [a]
chain 1 = [1]
chain n
  | odd n = n : chain (n*3 + 1)
  | otherwise = n : chain (n `div` 2)


-- find all chains w/ length > 15 for starting numbers 1 to 100

numLongChains :: Int
numLongChains = length (filter islong (map chain [1..100]))
  where islong x = length x > 20

-- a list of functions

listOfFuns = map (*) [0..]

-- LAMBDAS
-- lambdas are anonymous functions

a = zipWith (\ a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]

-- lambdas can pattern match

b = map (\ (x,y) -> x + y) [(1,3), (4,5), (6,7)]
--b = [4,9,13]

-- FOLDS
-- reduce a list to a single value

-- sum using fold

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\x y -> x + y) 0 xs

-- or

sum2 :: (Num a) => [a] -> a
sum2 xs = foldl (+) 0 xs

-- left fold elem function

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

-- Note, the type of accumulator and the end value is the same

-- RIGHT FOLDS
-- whereas whereas left fold's binary function has the accumulator as
-- the first value and the current value second, right fold has
-- the accumlator second and the current as the first parameter
-- i.e. foldr (+) 0 [1,2,3,4] is equiv to 1 + (2 + (3 + (4 + 0)))

mapRFold :: (a -> a) -> [a] -> [a]
mapRFold f xs = foldr (\ x acc -> (f x : acc)) [] xs

-- foldl1 and foldr1 work like the normal fold except you don't
-- need to give them an explict start value.  They assume the first
-- (or last) element is the starting value

sum'' xs = foldr1 (+) xs

-- some standard lib functions using folds

maximum' :: (Ord a) => [a] -> a
maximum' = foldl1 (\acc x -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' =  foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (\acc x -> x * acc)

foldFac :: (Enum a, Num a) => a -> a
foldFac n = foldl1 (*) [1..n]

filterFold :: ( a -> Bool) -> [a] -> [a]
filterFold p = foldr (\x acc -> if p x then (x : acc) else acc) []

-- NB
headFold :: [a] -> a
headFold = foldr1 (\x _ -> x)

lastFold :: [a] -> a
lastFold = foldl1 (\ _ x -> x)

-- a right fold over a list [3,4,5,6] with starting value z
-- looks like fold f z [3,4,5,6] (f 3 (f 4 (f 5 f( 6 z))))
-- a left fold, foldl g z [3,4,5,6] is equiv to  g (g ( g ( (g z 3) 4) 5) 6)
-- foldl (flip :) [] [3,4,5,6] is
-- flip (:) (flip (:) (flip (:) (flip : 3 [] ) 4) 5) 6


-- SCANS
-- reports intermediate accumulator states in fthe form of a list
seq = scanl (+) 0 [1,2,3,4,5]
-- [0,1,3,6,10,15]
-- seq is the sequence:
--      seq_0 = 0; seq_n = seq_n-1 + n

seqr = scanr (+) 0 [1,2,3,4,5]
-- [15,14,12,9,5,0]

seql1 = scanl1 (\acc x -> if x > acc then x else acc) [3,4,5,3,7,9,2,1]
-- [3,4,5,5,7,9,9,9]

flipScanl = scanl (flip (:)) [] [3,2,1]
--[[],[3],[2,3],[1,2,3]]

-- in scanl, final result is the last element of resulting list,
-- in scanr, it is the first element.

perm sigma elem = if length sigma < elem
  then error "out of bounds"
  else sigma !! (elem-1)

-- Function application with $
-- defined:
--    ($) :: (a -> b) -> a -> b
--    f $ x = f x
-- Function application with a space is left-associative
-- Function application with $ is right associative
-- sqrt ( 3 + 4 + 5) can be written sqrt $ 3+4+5
-- sum (filter (>10) (map (*2) [2..10])) could be written as
-- sum $ filter (>10) $ map (*2) [2..10]

-- map ($ 3) [(4+), (10*), (^2), sqrt]

-- FUNCTION COMPOSITION

--  (.) :: (b -> c) (a -> b) -> a -> c
--  f . g = \x -> f ( g x)


-- We could write:
-- negateList xs = map (\x negate (abs x) xs
-- or
negateList :: Num a => [a] -> [a]
negateList xs = map (negate . abs) xs

-- we can rewrite
-- replicate 100 (product (map (*3) (zipWith max [1,2,3,4,5] [4,5,6,7,8])))
-- as
-- replicate 100. product. map (*3) . zipWith max [1,2,3,4,5] $ [4,5,6,7,8]

-- a function is written in point-free style if it omits a parameter
-- eg, sum xs = foldl (+) 0 xs can be re-written as sum = foldl (+) 0
-- so we could rewrite
-- fn x = ceiling (negate (tan (cos (max 50 x)))) as
fn = ceiling . negate . tan . cos . max 50 
