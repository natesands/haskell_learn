
-- Nate Sands
-- 1 March 2020
-- CSc510003
-- Prof. Troeger
-- CCNY

-- TWO VERSION OF REVERSE --


{-
Write a function reverse that takes a list and returns it
in reversed order.

Write two versions, one in linear time, one in quadratic time.
-}

-- Recursive version in quadratic time

recReverse :: [a] -> [a]
recReverse [] = []
recReverse lst = last lst : recReverse (init lst)

{-
Proof
------
Note that when init is called on a singleton list [x]
it returns the empty list.  Hence recReverse [x] returns
x : [], i.e. [x], which is the reverse of the singleton list.

Assume that recReverse called on the list L[1..(k-1)] returns
the reversed list L[(k-1)..1].  Then
recReverse L[1..k] ==
L[k] : recReverse L[1..(k-1)]  ==
L[k] : L[(k-1)..1] ==
L[k..1]

Time complexity
---------------
The run time of recReverse depends on the implementations
of init and last:

last [x] = x
last (_:xs) = last xs
last [] = error "empty"

init [x] = []
init (x:xs) = x : init xs
init [] = error "empty"

Both run in Theta(n) time.

So recReverse's complexity is given by:
T(n) = Theta(n) + T(n-1)
or,
T(n) = Theta(n^2)
*****************

Now we want to consider possibilities for a linear version of reverse.


One hypothetical model is a function that employs an accumulator list
A.  The reverse of a list L is created by successively
breaking off the first element of L and cons'ing it to A,
i.e, if
A = [] and L = [a, b, c, d],
then,
[] [a,b,c,d] ->
a : [] [b,c,d,e] ->
b : a : [] [c, d]  ->
c : b : a : [] [d] ->
d : c : b : a: [] [] == [d,c,b,a] []
The function stops when L == [] and returns A.
-}

-- linear iterative reverse

reverseList :: [a] -> [a]
reverseList xs = reverseAcc [] xs

reverseAcc :: [a] -> [a] -> [a]
reverseAcc acc [] = acc
reverseAcc acc (x:xs) = reverseAcc (x : acc) xs

{-
Proof:
------
We take as our invariant that at the start of each call to
reverseAcc, reversed(xs) ++ acc = reversed(XS)
[where XS was our original list].
This is certainly true when acc = [] and xs = XS.
Now when acc=[x_k, x_k-1,..x_1], and xs = [x_k+1,..., x_n],
reverseAcc acc xs = reverseAcc [x_k+1,...,x_1] [x_k+2,...,x_n],
where the arguments satisfy the gInv. 
The function calls terminate when xs = [], then gInv
shows that [] + acc = acc = reversed(XS).

Time complexity.
---------------

T(n) = Theta(1) + T(n-1)
=> T(n) = Theta(n)



***  look into use of foldl, foldr, and flip w/ respect to reverse;
also ariticle at wiki.haskell.org/Foldr_Foldl_Foldl' ***
