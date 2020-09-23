-- RECURSION
-- When writing a recursion, the first question to ask...
-- "WHAT IS THE EDGE CASE?"



maximum' :: (Ord a) => [a] -> a
maximum' [] = error "empty list"
maximum' [x] = x
maximum' (x:xs) = if x > maximum' xs then x else maximum' xs

myMax :: (Ord a) => [a] -> a
myMax [] = error "empty list"
myMax [x] = x
myMax (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = myMax xs

myMax2 :: (Ord a) => [a] -> a
myMax2 [] = error "empty list"
myMax2 [x] = x
myMax2 (x:xs) = max x (myMax xs)

replicate' :: (Num i, Ord i) => i -> a -> [a]
-- note Num is not subclass of Ord, so Num doesn't have to adhere
-- to ordering.  so we have to specify both
replicate' n x
  | n <= 0 = []
  | n == 1 = [x]
  | otherwise = x : replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

elem' :: (Eq a) => a -> [a] -> Bool
elem' x [] = False
elem' x (y:ys)
  | x == y = True
  | otherwise = elem' x ys

-- QUICKSORT!!!!

quicksort :: (Ord a) => [a] -> [a]
-- edge case:
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <-xs, a <= x]
      biggerSorted = quicksort [a | a <-xs, a > x]
  in  smallerSorted ++ [x] ++ biggerSorted
  
