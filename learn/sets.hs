import qualified Data.Set as Set

{-

Sets are ordered, and they are unique.

The module provides set operations:


The type of Set.fromList [8,2,3,4] is Set.Set Int (Not [Int])

Set.fromList <list> - removes all duplicates and reorders
Set.toList <set> - converts back into a list

let's see if this works:

Set.toList $ Set.fromList [1..]

does not

λ> [1..] == [1..]
Interrupted.

