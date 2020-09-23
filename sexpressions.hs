
{-
   An implementation of sexpressions -- DRAFT
   D. Troeger
   April 2020
-}



import Data.Maybe



data Sexp a = Atom a
            | List [Sexp a]
            deriving (Show,Read,Eq)


{-

  Here is an example  --

-}


example = List [c1,c2]
              where
                 c1 = Atom "an"
                 c2 = List [c21, c22, c23]
                      where
                        nil =  List []
                        c21 = List [ List [ Atom "s-list"]]
                        c22 = List [ Atom "with", nil, Atom "lots"]
                        c23 =  List [ List [Atom "of"], Atom "nesting"]

{-

  Noting that [] is indeed a list of Sexps, a natural choice for nil is List [].
  But care is required in defining car, cdr and cons. 

-}

nil = List []


{-

  car, cdr, cons

-}

maybeCar :: (Eq a) =>Sexp a -> Maybe(Sexp a)
maybeCar (Atom x) = Nothing
maybeCar (List x) = if (x == []) then Nothing else Just (head x)


car x = fromMaybe (error "car") (maybeCar x)


maybeCdr :: (Eq a) => Sexp a -> Maybe(Sexp a)
maybeCdr (Atom x) = Nothing
maybeCdr (List x)
  | x == [] = Nothing
  | otherwise = Just (List (tail x))

cdr x = fromMaybe (error "cdr") (maybeCdr x)

                    
cons :: (Eq a) => Sexp a -> Sexp a -> Sexp a
cons x (List z) = List (x : z)





{-

  examples

  cons (Atom "an") nil = List [Atom "an"]

  car (cons (Atom "an") nil) = Atom "an"

  cdr (cons (Atom "an") nil) = List []

  cons (Atom 1) (cons (Atom 2) nil) == List [ Atom 1, Atom 2]

-}



{-

  Some test data

-}


t1 = cons (Atom 2) (cons (Atom 3) nil)
t2 = cons (Atom 1) (cons t1 nil)
t3 = cons (Atom 4) nil
t4 = cons (cons t2 nil)  t3



sexpLength :: Sexp a -> Int 
sexpLength (Atom x) = 0
sexpLength (List []) = 0
sexpLength (List (x:y)) = 1 + sexpLength (List y)


sexpFringe :: Sexp a -> [a]
sexpFringe (Atom x) = [x]
sexpFringe (List []) = []
sexpFringe (List ((Atom x) : y)) = [x] ++ sexpFringe (List y)
sexpFringe (List ((List x) : y)) = sexpFringe (List x) ++ sexpFringe (List y)


{-

 Examples for sexpFringe

-}

t5 = List ( (Atom 1) : [(Atom 2)])
   --  sexpFringe  t1
   --  [1,2]

t6 = cons (Atom 1) (cons (Atom 2) (cons (Atom 3) nil))
   -- sexpFringe t2
   -- [1,2,3]

t7 = cons t6 (cons (Atom 4) (cons (Atom 5) nil))
   --  sexpFringe t3
   -- [1,2,3,4,5]

t8 = cons t6 (cons t7 (cons t5 t6))
   -- sexpFringe t4
   -- [1,2,3,1,2,3,4,5,1,2,1,2,3]


{-

  A few other tree recursions

-}

sexpLeafCount :: Sexp a -> Int
sexpLeafCount (Atom x) = 1
sexpLeafCount (List []) = 0
sexpLeafCount (List ((Atom x) : y)) = 1 + sexpLeafCount (List y)
sexpLeafCount (List ((List x) : y)) = sexpLeafCount (List x) + sexpLeafCount (List y)


-- of course also
sexpLeafCount' :: Sexp a -> Int
sexpLeafCount' x = length (sexpFringe x)


{-
  Viewing Sexps as trees in the sense of my Lecture 9, and counting all nodes -- both
  internal and leaves.
-}


-- useful for understanding the top-level structure of a tree

maybeComps :: Sexp a -> Maybe ([Sexp a])
maybeComps (Atom x) = Nothing
maybeComps (List x) = Just x

comps x = fromMaybe (error "comps") (maybeComps x)
numComps x = length $ comps x




sexpNodeCount :: Sexp a -> Int
sexpNodeCount (Atom t) = 1
sexpNodeCount (List []) = 0
sexpNodeCount (List t) = (1 + (foldr (+) 0 (map sexpNodeCount t)))


{-

  sexpNodeCount t5
  3

  sexpNodeCount t6
  4

  numComps t7
  3

  sexpNodeCount t7
  7

  numComps (cons t5 t6)
  4

  numComps (cons t7 (cons t5 t6))
  5

  sexpNodeCount (cons t7 (cons t5 t6))
  14


-}



-- we can compute the sum of the degrees of all the nodes of trees represented this way


sexpDegreeSum :: Sexp a -> Int
sexpDegreeSum (Atom t) = 0
sexpDegreeSum (List []) = 0
sexpDegreeSum (List ((Atom x) : y)) = 1 + sexpDegreeSum (List y)
sexpDegreeSum (List ((List x) : y)) = 1 + sexpDegreeSum (List x) + sexpDegreeSum (List y)


-- reverse

maybeReverse :: (Eq a) => Sexp a -> Maybe(Sexp a)
maybeReverse (Atom x) = Nothing
maybeReverse (List x) = Just (List (reverse x))

sexpReverse x = fromMaybe (error "reverse") (maybeReverse x)



-- deep reverse

sexpDeepReverse :: (Eq a) => Sexp a -> Sexp a
sexpDeepReverse (Atom x) = Atom x
sexpDeepReverse (List x) = List (reverse (map sexpDeepReverse x))

