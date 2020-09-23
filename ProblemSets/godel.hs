-- Nate Sands
-- 25 April 2020
-- CSc510003
-- Prof. Troeger
-- CCNY

{- Contains
   1) example of a foldr/unfoldr inverse pair: godelEnc and primeFactorize,
   2) their use in a primative Gödel numbering map string -> integer,
   3) a fixed version of the Collatz sequence function w/out append 1
-}

import Data.List (unfoldr)
import Data.Char (ord, chr)

-------------------------------------------
-- Misc integer/prime number functions

-- counts number of times p divides n
countFactor :: Integer -> Integer -> Integer
countFactor p n
  | p == 1 = error "1 not a legal factor"
  | n `mod` p /= 0 = 0
  | otherwise = 1 + countFactor p (n `div` p)

-- removes every factor of p from n
removeFactor :: Integer -> Integer -> Integer
removeFactor p n = let fact = countFactor p n
  in n `div` p^fact

integersFrom :: Integer -> [Integer]
integersFrom k = k : map (+1) (integersFrom k)

sieve (s:tream) = s : (sieve (filter (\x -> not (x `mod` s == 0)) tream))
primes :: [Integer]
primes = sieve $ integersFrom 2

nthPrime :: Int -> Integer
nthPrime n = head $ drop (n-1) primes

-- 1 is not allowed to be an element of xs
countFactors n xs = unfoldr (\i -> if i == (length xs) then Nothing else Just(countFactor (xs !! i) n, i+1)) 0

-----------------------------------------------------------

-- Reduce n by dividing out all integer factors listed in xs
reduce :: Integer -> [Integer] -> Integer
reduce n xs = foldr (\x acc -> removeFactor x acc ) n xs

----------------------------------------------------------

{- The next two functions are inverses of each other using folds -}

-- primeFactorize returns "coordinates" of a number in terms
-- of its prime factors
-- e.g. 2 = [1], 3 = [0,1], 5 = [0,0,1], 60 = [2,1,2]
primeFactorize :: Integer -> [Integer]
primeFactorize num = unfoldr (\(n,i) -> if n == 1 then Nothing
                               else
                                 Just(countFactor (nthPrime i) n,
                                      (removeFactor (nthPrime i) n, i+1))) (num,1)

----------------------------------------------------------
-- godelEnc is a Pairing function from N x N x ...x N -> N
-- that takes a list of n integers and encodes them in the product
-- of powers of the first n primes.
-- e.g. [8,16,3] -> 2^8 * 3^16 * 5^2
-- This function is a bijection by the Fundamental Theorem of Arithmetic
godelEnc :: [Integer] -> Integer
godelEnc xs = foldr (\(a,b) x -> x*(a^b)) 1 zs
  where zs = zip (take l primes) xs
        l = length xs

-- godelEnc and primeFactorize are inverses of each other.  
-- primeFactorize $ godelEnc [2,1,2] = [2,1,2] , and
-- godelEnc $ primeFactorize 300 = 300

-- We can use godelEnc and primeFactorize to encode strings.
-- Note: ord takes char to its ASCII value,  and vice-versa
stringToInts :: [Char] -> [Integer]
stringToInts [] = []
stringToInts (s:tring) = toInteger (ord s) : stringToInts tring

stringFromInts :: [Integer] -> [Char]
stringFromInts [] = []
stringFromInts (x:xs) =  (chr $ fromInteger x) : stringFromInts xs

encodeString :: [Char] -> Integer
encodeString string = godelEnc  $ stringToInts string

decodeString :: Integer -> [Char]
decodeString n = stringFromInts $ primeFactorize n

{-
λ> a = encodeString "Haskell"
λ> a
1370846838365738931399396140892557031015883951466318030304257019077310821305478693908570203230303696883090177850051958287720642716727193615465994165499629721924678952912007428380776902945113121044148124679130284272576530320413061852436424716314027949626711737113735257793569300499031556509312868221061236496061231298306216830344916684872051391523736972988170652708691149626572106484540397318058245086934326598445623652028716024307272434289669601379701888930754357334416254135378560892381397095549573350581340491771697998046875000000000000000000000000000000000000000000000000000000000000000000000000
λ> decodeString a
"Haskell"
-}

----------------------------------------------------------

-- This function fixes the previous collatz function so it
-- includes 1 at the end of the fold, instead of having to
-- append it.
collatz' :: Int -> [Int]
collatz' n = unfoldr (\(x,lastNumWasOne) -> if lastNumWasOne == True
                       then Nothing
                       else Just(x, (nextTerm x, x == 1))) (n, False)
             where nextTerm :: Int -> Int
                   nextTerm c
                     | odd c = (3*c + 1)
                     | otherwise = c `div` 2

------------------------------------------------------------



