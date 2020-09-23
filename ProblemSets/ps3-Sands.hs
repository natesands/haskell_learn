-- Nate Sands
-- 26 Feb 2020
-- CSc510003
-- Prof. Troeger
-- CCNY

-- PROBLEM SET 3 --

{-Problem 1
  Implement mergesort. Do not assume that the input is a list of numbers.  
  Prove that your function works.
-}

mergeSort :: (Ord a) => [a] -> [a]
mergeSort xs
  | xs == [] = []
  | (length xs == 1) = xs
  | otherwise = merge (mergeSort (left xs)) (mergeSort (right xs))

--recursive merge
merge :: (Ord a) => [a] -> [a] -> [a]
merge xs [] = xs
merge [] xs = xs
merge (x:xs) (y:ys)
  | x < y = x : (merge xs (y:ys))
  | otherwise = y : (merge (x:xs) ys)

--iterative merge
mergeIter acc [] [] = acc
mergeIter acc xs [] = acc ++ xs
mergeIter acc [] ys = acc ++ ys
mergeIter acc (x:xs) (y:ys)
  | x < y = mergeIter (acc ++ [x]) xs (y:ys)
  | otherwise = mergeIter (acc ++ [y]) (x:xs) ys
  
-- pre: length xs > 1
left xs = [ xs !! i | i <- [0..p] ]
    where p = ((length xs) `div` 2) - 1

-- pre: length xs > 1
right xs = [ xs !! i | i <- [p+1..((length xs) -1)] ]
    where p = ((length xs) `div` 2) - 1
  

-- Proof of mergesort is  by strong induction.
-- A singleton list is already sorted and mergesort correctly
-- handles this case.  Now we assum mergesort 
-- correctly sorts any list of length k or less.  When the
-- function is called on a list of length k+1, it performs
-- mergesort on two smaller lists, each of size less than k, which
-- returns them sorted according to the inductive hypothesis.
-- If mergeIter  performs correctly, then these are joined into
-- a correctly sorted list of size k+1. Since 

-- The mergeIter function take two lists xs and ys and an
-- empty accumulator list acc.  Because mergesort has already been
-- performed on xs and ys, they are each in sorted order.  We choose
-- our loop invariant as follows: after the kth iteration, the elements
-- of acc are in sorted order, and each element in acc is less
-- any element in either xs or ys.  Since xs and ys are in sorted
-- order, the first element x in xs is less than or equal to the
-- rest of the elements in xs, and likewise for the first element
-- y in ys.   mergeIter takes the smallest of these two elements
-- x and y and appends it to acc.  This maintains the loop invariant.
-- Notice that the loop invariant is vacuously true before the first iteration
-- since acc is empty.  The program terminates when xs and ys are
-- empty and all elements have been placed in acc.  By the invariant
-- acc is then a sorted list combining the oringial elements of
-- xs and ys.  

{-Problem 2
  Same for insertionsort.  Give recursive and iterative versions.
-}

-- Helper function that adds an element to an already sorted list
-- pre: xs is a sorted list
insertOne xs x = takeWhile (< x) xs ++ [x] ++ dropWhile (< x) xs

{- iterative insertion sort -}
-- move "cards" from "deck" to "hand"
insertionSortI :: (Ord a) => [a] -> [a]
insertionSortI xs = insert [] xs
insert [] [] = []
insert hand [] = hand
insert hand (d:eck) = insert (insertOne hand d) eck

{- recursive insertion sort -}
insertionSort :: (Ord a)=> [a] -> [a]
insertionSort [] = []
insertionSort xs
  | length xs == 1 = xs
  | length xs == 2 = minimum xs : maximum xs : []
  | otherwise =  insertOne (insertionSort (init xs)) (last xs)
  
{-Problem 3
  Same for selectionsort.  Give recursive and iterative versions.
-}

{-recursive selection sort-}
selectionSort [] = []
selectSort xs = minimum xs : selectSort (remove (minimum xs) xs)
  where remove x xs = takeWhile (/= x) xs ++ tail (dropWhile (/=x) xs)

{- iterative selection sort -}
selectionSortI xs = selectIter [] xs
selectIter [] [] = []
selectIter hand [] = hand
selectIter hand deck =
  selectIter (insertOne hand nextMin) (remove nextMin deck)
  where nextMin = minimum deck
        remove x xs = takeWhile (/= x) xs ++ tail (dropWhile (/=x) xs)


{-Problem 4
  Write iterative and recursive Haskell functions to return the sum of the
  digits in a non-negative integer.  For example, the sum of the digits in
  345 is 12.  Prove your functions correct.
-}
sumDigits n
  | n < 10 = n
  | otherwise = (n `mod` 10) + (sumDigits (n `div` 10))

sumDigitsI n = sumDigitsIter n 0
sumDigitsIter n sum
  | n == 0 = sum
  | otherwise = sumDigitsIter (n `div` 10) ( sum + (n `mod` 10))

{- Problem 5
  Write iterative and recursive Haskell functions to test whether the digits
  in a non-negative integer are in increasing order.  For example, the 
  digits of 12348 are in increasing order, while those of 12343 are not.  Prove
  your functions correct.
-}
increasingOrder 0 = True
increasingOrder num = (lastDigit >= nextToLast) && increasingOrder numDivTen
  where lastDigit = num `mod` 10
        nextToLast = (num `div` 10) `mod` 10
        numDivTen  = num `div` 10

-- iterative version
-- this is not strictly an iterative  version since it
-- depends on a recursive auxilliary function

numberOfDigits n
  | n < 10 = 1
  | otherwise = 1 + numberOfDigits (n `div` 10)

increasingOrderI num = and [ (num `mod` i*10) >= ((num `mod` i*100) `div` 10) |
                             i <- [1..((numberOfDigits num) - 1)] ]
                             

{-
* Problem 6
  Solve Exercise 1.12 in Abelson and Sussman, using Haskell, and (proper) 
  recursion. Next, give an iterative solution to the same 
  exercise, using lists.  Give proofs for both programs.
-}

-- The exercise asks to write a function to compute the entries in
-- Pascal's triangle.
-- We impose a coordinate system on Pascal's Triangle using a
-- i-axis that runs along the left border of 1's and a j-axis
-- along the right border, with (0,0) being the entry at the
-- top of the triangle.  Any entry can be identified with
-- a pair (i,j).  Using the property that any entry with i=0
-- or j = 0 has a value of 1, and that every other entry is the sum
-- of the two entries above it gives us the following recursion:

ijEntryPT _ 0 = 1
ijEntryPT 0 _ = 1
ijEntryPT i j = (ijEntryPT (i-1) j) + (ijEntryPT i (j-1))

-- Note that in row k of PT, the coordinates of the entries going
-- from left to right are (k,0), (k-1,1), ... (k-l, l), ..., (0,k).
-- So we can generate a list containing the entries in any row:

kthRowPT k = [ ijEntryPT (k -1 - l) l | l <- [0..(k-1)] ]

-- or the first n rows:

firstNRowsPT n = [ kthRowPT i | i <- [1..n] ]  

-- For an iterative version, we want to make the relationship
-- of the entries of Pascal's triangle to the binomial coefficients
-- more explicit, (without resorting to computing factorials).
-- So we drop the coordinate system and adopt the
-- convention that [n k] represents the kth entry of the nth row.
-- But here we count the first row as row 0, and the first entry
-- of any row as entry 0.  
-- We define [n 0] = 1 for any n, and use the property that the
-- kth entry of any row n is given by
--                  [n k] = [n k-1] * (n + 1 - k) / k
-- Because we compute an entry using the result of the previous entry
-- this suggest using a fold.

nchoosek _ 0 = 1
nchoosek n k
  | k > n = error "entry given is too big for that row!"
  | otherwise = foldl (\x y -> x * (n+1- y) /  y) 1 [1..k]

-- we construct a list containing the coefficients in (x + y)^n

binomCoeffN n = [ nchoosek n k | k<-[0..n] ]

--  set up a function to compute (x + y)^n

terms x y n = [ (x^(n-k))*(y^k) | k <-[0..n] ]
xPlsY2N x y n = sum( zipWith (*) (binomCoeffN n) (terms x y n))
        

{-
* Problem 7
  Write iterative and recursive Haskell functions to replace every occurrence
  of an element (call it old) by another element (new) in a list.  Give proofs
  for both programs.
-}

--recursive
rplOldNew :: (Eq a) => a -> a -> [a] -> [a]
rplOldNew old new [] = []
rplOldNew old new (x:xs) = (if x == old then new else x) : rplOldNew old new xs

--iterative
rplOldNew old new list = rpl old new [] list 
rpl _ _ acc [] = acc
rpl old new acc (x:xs) = if x == old then rpl old new (new:acc) xs
                         else rpl old new (x:acc) xs
{-
* Problem 8
  Does Haskell in fact support tail recursion?  See what you can find out!

Because Haskell uses lazy evaluation, the operations that are done inside
the parameters of a tail-recursive function, such as one in which an accumulator
is used, are delayed until the final function call.  This can cause
instructions to accumulate on the stack much like they do in proper
recursive functions.

-}
