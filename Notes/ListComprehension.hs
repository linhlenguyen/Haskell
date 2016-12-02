{--
down vote
To augment augustss's answer, if you have something like:

[(x, y) | x <- [1..3], y <- [1..3], x + y == 4]
... it is equivalent to this use of do notation:

do x <- [1..3]
   y <- [1..3]
   guard (x + y == 4)
   return (x, y)
... which is equivalent to this use of concatMap:

concatMap (\x ->
    concatMap (\y ->
        if (x + y == 4) then [(x, y)] else []
        ) [1..3]
    ) [1..3]
--}
