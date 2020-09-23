data OC a = Nil | Node a (OC a) | Open (OC a) | Close (OC a)

-- ()
l0 = Nil
-- (1)
l1 = Open (Node 1 (Close Nil))
-- (1 2)
l2 = Open (Node 1 (Node 2 (Close Nil)))
-- (1 (2 3))
l3 = Open (Node 1 (Open (Node 2 (Node 3 (Close (Close Nil))))))
-- (1 (2 (3 4)))
l4 = Open (Node 1 (Open (Node 2 (Open (Node 3 (Node 4 (Close (Close (Close Nil)))))))))

car :: OC a -> OC a
