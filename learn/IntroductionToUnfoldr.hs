

{-
  I work here with the unfoldr function, defined in Data.List.

      The unfoldr function is a `dual' to foldr: while foldr reduces a list to a summary value, unfoldr
      builds a list from a seed value. The function takes the element and returns Nothing if it is done
      producing the list or returns Just (a,b), in which case, a is a prepended to the list and b is used
      as the next element in a recursive call. Some examples are given below.

  Reference

    https://downloads.haskell.org/~ghc/latest/docs/html/libraries/base-4.13.0.0/Data-List.html#g:9

  Other References

    https://wiki.haskell.org/Blow_your_mind


-}


import qualified Data.List as L (unfoldr)
import Data.Ratio

pow2 :: [Integer]
pow2 = L.unfoldr (\n -> Just (n, 2*n)) 1

fibs :: [Integer]
fibs = L.unfoldr (\(a,b) -> Just (a, (b, a+b))) (0,1)


{-
  so a is pre-pended to the output, and the next instantiation of (a,b) is (b, a+b).

  What happens next?  b is prepended to the (rest of the) output, and the next instantiation of the input is (a+b, b + (a + b)).
  So the first three elements of the returned list will be a, b, a+b. 

-}


{-

  here are some additional examples, all producing infinite lists

-}


-- different formulations for the harmonic sequence


har :: [Rational]
har = L.unfoldr (\(m,n) -> Just ((1%n), (m, n+1))) (1,1)

har' = L.unfoldr(\(m,n) -> Just ((m%n), (m, n+1))) (1,1)

har'' = L.unfoldr(\n -> Just ((1%n), n+1)) 1



-- the piSum example from Abelson and Sussman

piSum :: Int -> Double
piSum = \m -> 8 * (sum (take m (L.unfoldr (\n -> Just( (1/(n * (n + 2))), n+4)) 1)))


-- one guesses that Prelude's iterate function could be defined in terms of unfoldr (though it is not)

myIterate :: (a -> a) -> a -> [a]
myIterate f init = L.unfoldr( \x -> Just (x, f x)) init

square x = x * x

squares = myIterate square 2




{-

  Can unfoldr produce a finite list?  One example is given in Data.List:

    unfoldr (\b -> if b == 0 then Nothing else Just (b, b-1)) 10
  giving
    [10,9,8,7,6,5,4,3,2,1]

  Similarly, if n is a non-negative integer:

-}

lstOfNums n = L.unfoldr (\b -> if b == (n + 1) then Nothing else Just (b,b+1)) 0



{-

  Now wait -- is this really similar to the previous example?  Note that n is global in the function argument
  (call it f) to unfoldr.  This feels like a violation of modularity.  Yet somehow the stopping condition has to be
  made visible.  We emulate earlier examples, and make the argument to f a tuple.

-}


listOfNums' n = L.unfoldr(\(a,b) -> if b == 0 then Nothing else Just (a, (a + 1, b - 1))) (1, n)


{-

  OK - so modularity is restored.  But we are still counting down.  Can we have a modular counting up
  design?  Of course:

-}


listOfNums'' n = L.unfoldr(\(a,b) -> if a == b + 1 then Nothing else Just (a, (a + 1, b))) (1,n)




{-

  Data.list observes that, for some functions, unfoldr can "undo" foldr.  As dualities like this occur
  throughout mathematics, this caught my eye.  


  An easy example showing that unfoldr can undo foldr is obtained when both transformations are the identity.
  For example,  with ns and ms as follows


-}


ns lst = L.unfoldr(\(xs) -> if (xs == []) then Nothing else Just (head xs, tail  xs)) lst


ms lst = foldr (:) [] lst




{-

  we have ns $ ms lst == lst, and indeed as well ns $ ms lst == lst.  Not so impressive, but it is
  interesting to observe that the function arguments fn and fm to unfoldr in ns and ms, respectively,
  satisfy the condition given in Data.list for unfoldr fn (foldr fm z xs) == xs
  namely fn (fm x y) = Just (x,y) AND fn z = Nothing

-}



{-
  Notice as well for future reference that the following works

    ns = 
      let 
        aux [] = Nothing
        aux xs = Just (head xs, tail xs)
      in
        L.unfoldr (\(xs) -> aux xs) [1..10]



  Nate gave a very nice demonstration of further syntactic possibilities:


-}

collatz n = col n ++ [1]
col n  = L.unfoldr (\a -> if a == 1 then Nothing
                       else Just (a, nextTerm a)) n
           where nextTerm c
                   | odd c = (3*c + 1)
                   | otherwise = c `div` 2



{-
  What about a more interesting foldr/unfoldr inverse pair?  Consider 
-}


t1 init = L.unfoldr(\(a,b) -> if b == 0 then Nothing else Just (a, (a+1, b - a))) (1,init)

s1 lstOfNums = foldr (+) 0 lstOfNums


{-
  Here t1 $ s1 lstOfNums == lstOfNums.  Yet this is not quite in the right form, since
  init has to be input to L.unfoldr as the tuple, (1,init).  We might
  adjust as follows
-}

t2 :: (Eq a, Num a) => (a, a) -> [a]
t2 pair =  L.unfoldr(\(a,b) -> if b == 0 then Nothing else Just (a, (a+1, b - a))) pair



s2 :: (Foldable t1, Num a, Num t) => t1 t -> (a, t)
s2 lstOfNums = foldr strangePlus (1,0) lstOfNums
               where
                 strangePlus = (\a b -> (1, a + (snd b)))


{-
  And now t2 (s2 [1..10]) == [1..10] and s2 (t2 (1,55)) == (1,55), which is cool, but the use
  of strangePlus is wierd. 
-}


-----------------------------------------------------------------------------------------------------------

{-

  Let's try to say more clearly what is going on with unfoldr.

  first, unfoldr itself is a functional, say u, taking function argument f and returning a list.  The examples above suggest
  that f must have the form

     f = (\b -> if (stoppingTest b) then Nothing else (next b))

  where (next b) returns a pair Just (a, b').  The functional u will grab a, consing it to (the rest of)
  the output, and b' is the next value of the parameter b.  Thus if T is the type of a, [T] is the type of the value returned by u f,
  and u is essentially a while loop.

  Rewriting my last example makes this concrete:


-}


t3 pair =  L.unfoldr(\(a,b) -> if (stoppingTest (a,b))  then Nothing else (next (a,b))) pair
  where stoppingTest (a,b) = (b == 0)
        next (a,b) = Just (a, (a + 1, b - a))



{-
  
  The flexibility of this setup only became apparent to me when I finally noticed that the actual values of b
  can be tuples.  (Reminds me of the time I got stuck installing NetBSD on an old mac, in the late 90's:  it took me two
  days to notice that there was a button on the screen to do exactly what I could not figure out how to do ... I was going
  nuts. )

-}

{-

  If the stopping test is never satisfied, then we get infinite lists.  For example:

-} 

fibs' = L.unfoldr (\(a,b) -> if (stoppingTest (a,b)) then Nothing else Just (a, (b, a+b))) (0,1)
  where stoppingTest (a,b) = False


{-

  So it seems unfoldr regards the absence of a stopping test as equivalent to the identically False stopping predicate.  

-}



-------------------------------------------------------------------------------------------------------


{-

  Additional examples of unfoldr/foldr inverse pairs

  (Coming)

-}


















                  
