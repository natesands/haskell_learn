
-- Nate Sands
-- 8 April 2020
-- CSc510003
-- Prof. Troeger
-- CCNY

-- PROBLEM SET 5 --



import Data.Maybe
import qualified Data.Map as Map
import qualified Data.List as L (unfoldr)
import Data.Ratio
{- * The exercises in this group pertain to the section Data.Map of Chapter 7 (Modules): 

** How can a computation proceed, once a Maybe type has been returned in an intermediate step?  Take a look in Data.Maybe, and give a couple of simple examples showing how this could be done.
-}

-- Suppose we want a procedure that applies a function to the head of list,
-- but we want to avoid the error thrown when head is called on the empty list.

headMapF :: (a -> b) -> [a] -> Maybe b
headMapF _ [] = Nothing
headMapF f xs = Just (f $ head xs)

--e.g.

a = [[3,4,5], [2],[],[4,5],[3,5,6], []]

b = [ headMapF (*3) x | x <- a]

-- Now b is a list of type [Maybe Integer] which contains [Just 9,Just 6,Nothing,Just 12,Just 9].
-- We convert this list to actual integers with a NULL value to mark where empty lists occured.

c = let myNull = -99999 in
  [if isNothing x then myNull else fromJust x | x <- b]

-- Now c contains [9,6,-99999,12,9,-99999]

{-
** Miran Lipovaca (our author) gives a foldr version of his function findKey.  Explain how this version works, and give as well a version which uses foldl instead.
-}

-- book version of findKey:
findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing
-- The initial value of acc is Nothing.  The function is applied to a list of (k,v)
-- tuples.  Starting from the right of the list, it each tuple to see if it contains
-- a k equal to key.  If so, then acc is set to Just v.  When the procedure finishes,
-- it returns acc = Nothing if key was not found, or acc = Just v, corresponding to the
-- first (k,v) tuple in the list where k == key.

-- version using foldl:
findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' key = foldl (\acc (k,v) -> if key == k then Just v else acc) Nothing


{-
** Check that there is a difference between the Prelude lookup and the lookup function in Data.Map.  Do enough reading to give a brief explanation for this. 
-}

-- The data type for Prelude.lookup is
-- Eq a => a -> [(a, b)] -> Maybe b
-- whereas the type for Map.lookup is
-- Ord k => k -> Map.Map k a -> Maybe a

-- Both return values of type Maybe, but Prelude.lookup works on
-- lists of tuples, whereas Map.lookup uses the data structure Map.
-- Map requires the key value to be of type Ord.  This allows
-- a Map instance to be stored as a tree for efficiency, and also
-- enables functions such at Map.lookupGT, Map.lookupLS, etc.

{-
** What exactly is the fromList function?  The text says that it returns a map -- ok: can this map be applied?  If not, then how is it used?  Give an example by using fromList to define and use a function myMap.  Once you have myMap, define a function myMap' which extends your myMap. In fact, give a couple of examples -- note that the keys can be tuples, for example. 
-}

-- A map can be applied (like a function) to a value using the Map.! operator.
-- e.g.

squares = [(x,x^2) | x <- [1..200]]
squaresMap = Map.fromList squares

-- so...
-- squaresMap Map.! 3
-- returns 9

-- Note however that if the key value is not found, an error is returned "given key is not an element in the map"
-- Also, it doensn't appear as if maps support lazy evaluation in their construction..

{-
** Create a map f from (key,value) pairs in which the values are  functions - perhaps different for each pair, and then figure out whether it is possible to use this map so that on application, the function for a given key is applied to that key.  If so, set up a couple of examples.  If not, say what the problem is. 
-}

f x = func x
  where func = Map.fromList [(1, (*2)), (2, (*3)), (3,(*4))] Map.! x

{-
** There is an interesting variant on fromList, namely fromListWith.  Read up on fromListWith (in LYaHfGG, henceforth LYaH) and in  https://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Map-Strict.html#g:15 , and write a function pairsToMap which inputs a list xs of (key,value) pairs and an appropriate function f, and which returns a map with all values for a given key combined as per f.  
-}
-- "Ord b" is requred since keys are being compared 
pairsToMap :: (Ord b) => (a -> a -> a) -> [(b,a)] -> Map.Map b a
pairsToMap f xs = Map.fromListWith f  xs 

{-
Use Data.List.unfoldr to (i) generate the harmonic series (as an infinite list of exact fractions), (ii) to compute the successive approximations to pi/8 given in Section 1.3.1 in Abelson and Sussman (as doubles), and (iii) to implement a function myIterate.
-}

harmonicSeries :: [Rational]
harmonicSeries = L.unfoldr (\(sum, n) -> Just (sum, (sum + 1 % n, n+1))) (0,1)

piSeries :: [Double]
piSeries = L.unfoldr(\(sum,n) -> Just (sum, (sum + 1.0 / (n * (n+2)), n+4)) ) (0,1)

myIterate :: (a -> a) -> a -> [a]
myIterate f x = L.unfoldr (\x -> Just (x, f x)) x

-- Can unfoldr produce finite lists?
-- Answer:  Yes, with the appropriate termination condition specified.
-- e.g, suppose we want to calculate a Collatz sequence starting with integer n
collatz n = col n ++ [1]
col n  = L.unfoldr (\a -> if a == 1 then Nothing
                       else Just (a, nextTerm a)) n
           where nextTerm c
                   | odd c = (3*c + 1)
                   | otherwise = c `div` 2

-- These sequences are all finite (or are they?)

-- Can foldr work on an infinite list?
-- Answer:  No.
-- foldr takes a list and creates a chain of delyed computations, which
-- are not calculated until the end of the list is reached. For example,
-- the operation
-- foldr (*) 1 [2,3,4,5,6]
-- becomes
-- ( 1 * ( 2 * ( 3 * ( 4 * (5 * 6)))))
-- which is evaluated begining with rightmost operation (5*6), then moving
-- leftward through each nested caclculation.  An infinite list would
-- result in an infinite chain of delayed computations which would
-- never be evaluated.

{-We have already seen how to implement foldr in Scheme (Abelson and Sussman call it 'accumulate').  Develop one or two candidates for unfoldr (call it unfoldrS) in Scheme.  Answer the previous two questions for your versions of unfoldrS and accumulate.  You may find the attached file (streams.scm)  helpful.

(define (geta abtuple) (car abtuple))
(define (getb abtuple) (car (cdr abtuple)))

(define (unfoldb proc x)
  (cons-stream (geta (proc x))
               (unfoldb proc (getb (proc x)))))


-}





