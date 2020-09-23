{-# LANGUAGE ParallelListComp #-}
-- experiments with streams

ones :: [Integer]
ones = 1 : ones

nats n = n : [ a | a <- nats (n+1) ]

nats' n = n : nats' (n+1)

fibs = 0 : 1 : [ a + b | a <- tail $ fibs | b <- fibs ]

{-
1 1 2 3 5 8
0 1 1 2 3 5 +
---------
1 2 3 4 8 13
-}

fibs' = 0 : 1 : zipWith (+)  fibs' (tail fibs')

addStreams st1 st2 = zipWith (+) st1 st2

scaleStream stream factor = map (*factor) stream

double = 1 : scaleStream double 2

