
-- I stumbled across this implementation of reverse using foldr on the internet.

foldrReverse :: Foldable t => t a -> [a]
foldrReverse xs = foldr (\b g x -> g  (b : x)) id xs []

-- At first glance it seems like foldr has been given one too many arguments.  But in fact, the call to foldr returns a function which is then applied to the empty list '[]'.

f :: [Int] -> [Int]
f = foldr (\b g x -> g (b : x)) id [1,2,3]

--'f []' returns [3,2,1]

-- In detail, let's say we want to reverse the list [1,2,3] using foldrReverse.
-- f  = 
-- (\b g x -> g (b : x)) 1 (
--   (\b g x -> g (b : x)) 2 (
--     (\b g x -> g (b : x)) 3 
--      id))
-- =
-- (\b g x -> g (b : x)) 1 (
--   (\ b g x -> g (b : x)) 2 (
--     (\x -> id (3 : x)))
-- =
--(\b g x -> g (b : x)) 1 (
--  (\y -> (\x -> id (3 : x)) (2 : y)))
-- =
--(\z -> (\y -> (\x -> id (3 :x)) (2:y)) (1 : z))

-- Now if we apply the above to '[]' we get

--(\z -> (\y -> (\x -> id (3 :x)) (2:y)) (1 : z)) [] =
--(\y -> (\x -> id (3 : x)) (2:y)) (1:[]) =
--(\x -> id (3 :x)) (2:(1:[])) =
-- id (3:(2:(1:[]))) =
-- id [3,2,1] =
-- [3,2,1]

-- The time cost T(n) = theta(1) + theta(n-1) = theta(n)
