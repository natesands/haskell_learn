import Data.List
import Data.Char
import Data.Function -- has `on`?
import qualified Data.Map as Map
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

-- Note that > length . nub [1,1,3,4]
-- generates an error
-- but (length . nub) [1,1,3,4] returns 3
-- also, length . nub $ [1,1,3,4] correctly returns

-- recall f $ expression
-- evaluates expression which is then applied as
-- a parameter of the function
a = intersperse '.' "MONKEY"
b = intercalate " " ["hey", "there", "guys"]
trnsposeMatrix xxs = transpose xxs

addPolys :: (Num a) => [[a]] -> [a]
addPolys xxs = map sum $ transpose xxs

-- foldl' and foldr' are NON-LAZY versions of these
-- functions.  Prevent stack overflows

-- concat flattens a list of lists

-- note:  map (replicate 4) [1,2,3]
-- gives [[1,1,1,1],[2,2,2,2],[3,3,3,3]]

-- concatMap (replicate 4) [1,2,3]
-- maps replicate 4 to each element of list
-- then flattens it so we get
-- [1,1,1,1,2,2,2,2,3,3,3,3]

-- iterate f x
-- applies f x repeatedly and returns an infinite list with
-- results of successive applications
-- eg
-- λ> take 5 $ iterate (\x -> -x) 1
-- [1,-1,1,-1,1]

--splitAt n xs
-- splits list at n and returns tuple

--takeWhile, dropWhile

-- span p xs
-- returns pair of lists, first of which is what
-- would have been returned by takeWhile, second is
-- remainder

-- break p xs
-- returns two lists, the second of which begins with
-- the first element for which p was true

-- sort xs
-- sorts a list

-- group xs
-- returns list and groups into sublists equal adjacent members
{-
λ> l = [2,5,7,9,10,2,5,2,3,7,4,6,2,8,8]
λ> group . sort $ l
[[2,2,2,2],[3],[4],[5,5],[6],[7,7],[8,8],[9],[10]]
-}

-- side note on patterns using @

capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

-- this creates an alias for (x:xs)

countEntries xs = map (\l@(x:xs) -> (x, length l)) . group . sort $ xs

-- inits, tails
-- recursive apply init and tail to list

-- search for sublist
search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
  let nlen = length needle
  in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)
-- Note once acc = True, it stays that way

-- this is virtually the same as the function `isInfixOf`
-- related functions:  `isPrefixOf`, `isSuffixOf`
-- `elem` `notElem`

-- partition p xs
-- returns list of elements satisfying p, list not p

-- find p xs
-- returns first element that satisfies p
-- but in a Maybe value which can be Just <something>
-- or Nothing

{-
λ> find (>4) [1,2,34,5,67]
Just 34
λ> find (<3) [5,4,6,7]
Nothing
-}

-- elemIndex a xs
-- returns a Maybe value with Just the index of a or Nothing

-- elemIndices a xs
-- returns list of indices of occurrence of element a

-- findIndex p xs
-- retruns Maybe

-- findIndices p xs

-- zip3, zip4,...zipWith3... zipWith7

-- lines, unlines

--words, unwords

-- nub xs
-- delete elem xs

-- \\ differnece function, like set difference
-- but only removes one occurrence per element on right
{-
λ> "blah blah blah" \\ "hab"
"l blah blah"
-}

-- union is like set union
-- intersect like set intersection 

on' :: (b -> b -> c) -> (a -> b) -> a -> a -> c
on' g f x y = g (f x) (f y)

encode :: Int -> String -> String
encode shift msg =
  let ords = map ord msg
      shifted = map (+ shift) ords
  in map chr shifted

-- this is equivalent to
encode' shift msg = map (chr . (+ shift ) . ord) msg

decode shift msg = encode (negate shift) msg

-- find value in list of (key,value) pairs

findKey :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs) = if key == k
                         then Just v
                              else findKey key xs

-- written as a fold
findKey' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey' key  = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing
