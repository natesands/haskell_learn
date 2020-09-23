{- PROBLEM 1
Write a predicate compare which inputs lists a and b of numbers, and which checks
  whether the ith element of a is less than or equal to the ith element of b, for all i
  between 0 and m - 1, where m is the length of the shorter of the two lists.
-}
shorterList a b = if length a <= length b then a else b

myCompare a b = and [ (a !! i) <= (b !! i) | i <-[0..(length (shorterList a b)-1)] ]

a = [1, 2, 5, 4, 5]
b = [2, 3, 4]

{- PROBLEM 2
Write a function triples which inputs a positive integer n, and which outputs 
  the list of all 3-tuples (i,j,k) of distinct integers i, j and k, with 1 <= i <= n,
  1 <= j <= n, and 1 <= k <= n.
-}

triples n = [ (i, j, k) | i <- [1..n], j <- [1..n], k <- [1..n],
              i/=j, j/=k, i/=k ]

{- PROBLEM 3
 Write a function triplesSummingToS which inputs a positive integer n and an integer S,
  and which outputs the list of all 3-tuples (i,j,k) of distinct integers i,j and k as in Problem 1, 
  but with the added constraint that i + j + k = S
-}

sumTriple t = foldl (\x (i, j, k) -> i+j+k) 0 [t]  -- returns number

sumTriple2 t = map (\ (i, j, k) -> i+j+k) [t]  -- returns list

tripleSummingToS n s = [ triple | triple <- (triples n), (sumTriple triple) == s ]

{- PROBLEM 4
Write a function orderedTriples which inputs a positive integer n, and which outputs the list
  of all 3-tuples as in Problem 1, but with i < j < k.
-}

orderedTriples n = [ (i, j, k) | k <- [1..n], j <- [1..k-1], i <- [1..j-1] ]

{- PROBLEM 5
Use the prelude function foldr to write a function flatten, so that 
  flatten [[1],[2],[3]] = [1,2,3], and in general, if x is a finite list of finite lists xi,
  then flatten x is the list consisting of the elements of the xi, in order.
-}

flatten xxs =  foldr (\x y -> x ++ y) [] xxs

{- PROBLEM 6
 Write a function removeDuplicates which inputs a finite list x and returns a list y which contains
  exactly the elements of x, but with multiplicity 1.  So: removeDuplicates [1,1,2,1,2] = [1,2].
  Do not worry about the order of the elements.
-}

removeDuplicates list = foldl (\ x y -> if not (elem y x)
                                then y:x else x) [] list

{- PROBLEM 7
Write a predicate sameElements which inputs two finite lists m and n and checks whether m and n have the
  same elements (perhaps in different order).
-}

countInList n list = sum [ 1 | x <- list,  x == n]

isSublist a b = and [ (countInList n a) == (countInList n b) |
                     n <- a]
                
sameElements list1 list2 = and [ (isSublist list1 list2), (isSublist list2 list1)]

{- PROBLEM 8
  Write a predicate distinct which inputs a list of numbers and which checks whether all these numbers
  are distinct.
-}

distinct x = if length (removeDuplicates x) == length x
  then True else False

{- PROBLEM 9
 Write a function to solve this puzzle.  Baker, Cooper, Fletcher, Miller, and Smith live on different 
  floors of an apartment house that contains only five floors. Baker does not live on the top floor. 
  Cooper does not live on the bottom floor. Fletcher does not live on either the top or the bottom floor. 
  Miller lives on a higher floor than does Cooper. Smith does not live on a floor adjacent to Fletcher's. 
  Fletcher does not live on a floor adjacent to Cooper's. Where does everyone live? (Abelson and Sussman 
  attribute this puzzle to Dinesman)

-}

residents = ["Baker", "Cooper", "Fletcher", "Miller", "Smith"]

allCombos = [ [a, b, c, d, e] | a <- residents, b <-residents,
              c <-residents, d <-residents, e <- residents, distinct [a, b, c, d, e] ]

whatFloorIs name floorPlan = sum [ i | i <- [0..4], (floorPlan !! i) == name ]

myAbs a = if a >= 0 then a else -a

neighbors r1 r2 floorPlan = ( myAbs ((whatFloorIs r1 floorPlan) - (whatFloorIs r2 floorPlan)) ) == 1

okForBaker combo = (combo !! 4) /= "Baker"

okForCooper combo = ( (combo !! 0) /= "Cooper" ) &&
                    ( not (neighbors "Cooper" "Fletcher" combo))

okForFletcher combo = ( (combo !! 0) /= "Fletcher" ) &&
                      ( (combo !! 4) /= "Fletcher" )

okForMiller combo = (whatFloorIs "Miller" combo) > (whatFloorIs "Cooper" combo)

okForSmith combo = not (neighbors "Smith" "Fletcher" combo)

answer = flatten [ floorPlan | floorPlan <- allCombos,
           okForBaker floorPlan,
           okForCooper floorPlan,
           okForFletcher floorPlan,
           okForMiller floorPlan,
           okForSmith floorPlan ]

-- Answer in ascending floor order is:
-- Smith, Cooper, Baker, Fletcher, Miller
