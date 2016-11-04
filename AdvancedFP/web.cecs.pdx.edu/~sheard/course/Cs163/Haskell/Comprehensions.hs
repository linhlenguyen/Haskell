module Comprehensions where



-- sequences

x1 = [1..5]

x2 = [4..75464]

-- count by something other than 1
x3 = [0,5 .. 36]

-- count backwards

x4 = [10,9 .. 0]

x5 n = [100, 100-n .. 0]

-- Comprehensions with generators

x6 = [ x + 5 | x <- [1,2,6,8,33]]

x7 :: [(Int,Int)]
x7 = [ (i,j) | i <- [1..4], j <- [30..32] ]

x8 = [ i+j | i <- [1..4], j <- [30..32] ]

--  Guards

x9 = [ i+j | i <- [1..4], j <- [30..32] , even i ]

---------------------------------------------
-- Zipping

x10 = zip x4 [1 ..]


