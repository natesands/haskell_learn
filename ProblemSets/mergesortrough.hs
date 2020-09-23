-- assume length xs is a power of 2
-- mergesort i xs
-- size of blocks to be merged is 2^i, i from 0 to log length xs = n
-- invariant: each block is in sorted order
-- ends with single block of length xs in sorted order
--split list into blocks of size 2^i;
-- merge each consective pair into a sorted block of size 2^(i+1)
-- finally we are left with two blocks of size 2^(n-1) which we merge

-- the problem seems to depend on our ability to chop our orignal list
-- into sublists of size 2^i... then we choose them two by two, merge,..
-- maybe we just split list into singletons... take 2, merge
--- use a fold with ++ to rejoin into a full list.. 

-- reduce a list to a list of singletons

atomize xs = [ [x] | x <- xs ]

-- now the ideas is to take two elements at a time and combine

flatten xxs =  foldr (\x y -> x ++ y) [] xxs
{-  basic idea...

let xs be the list to be sorted.

for now we assume that the length of xs is some power of 2.

we let i be an index... i ranges from 0 to log (length xs).

take the elements of xs, divide them into lists of size 2^i.

Let our invariant be that for each i, each sublist of size
2^i is in sorted order.  When i reaches the value of  log (length xs), then
we have a we have a list of length xs in sorted order
(therefore xs is sorted).

method:

let length xs = 2^n
i=0
we take our original list xs and create a list of (2^n/2^0)  sublists each of size 2^0 = 1.

take two at a time, merge them.  we are left with a list of 2^n/2^1 = 2^(n-1)  sublists each of size 2^1

i= k
generate a list (2^n/2^k) = 2^(n-k)  sublists each of size 2^k.  Merge them two at a time.  We are left with a list of 2^n/(2^k) sublists
each of size 2^k.

-- create a list of singleton lists from original lists...inbox
outer loop: go from 0 to log (length inbox)
-- have an empty list ready to receive merged elements.. outbox
inner loop
-- take two lists from inbox, merge them, add the merged lists to outbox
-- drop two from inbox.. take the next two... merge and add to outbox
-- function finishes when inbox is empty lists

atomize inbox
length of inbox = 2^n

-- mergesort i inbox outbox
inv: inbox is list of length 2^(n-i) containing
     lists of size 2^i, each in sorted order.
 -- mergeByPair j numpairs inbox outbox
    inv :  there are (numpairs-j) pairs in outbox
           take pair, merge, add to outbox, remove from inbox
      ends when inbox = []
       call mergesort i++ outbox []

-}


-- **** ONLY WORKS ON LISTS OF LENGTH 8 ***
-- I need to set n to log2 of length ys
-- but I am having trouble converting floats
-- to ints!! 
mergesort ys =
  let n = 3 in
   flatten ( mergesortIter 0 n [] (atomize ys) )

mergesortIter i n xs ys
  | i == n = ys
  | otherwise = mergesortIter (i+1) n [] (doThing xs ys)

doThing xs ys
  | ys == [] = xs
  | otherwise = doThing (xs ++ [merged]) (drop 2 ys)
  where merged =
          let pair = take 2 ys in
            mergeIter [] (pair !! 0) (pair !! 1)
  
--iterative merge
mergeIter acc [] [] = acc
mergeIter acc xs [] = acc ++ xs
mergeIter acc [] ys = acc ++ ys
mergeIter acc (x:xs) (y:ys)
  | x < y = mergeIter (acc ++ [x]) xs (y:ys)
  | otherwise = mergeIter (acc ++ [y]) (x:xs) ys
