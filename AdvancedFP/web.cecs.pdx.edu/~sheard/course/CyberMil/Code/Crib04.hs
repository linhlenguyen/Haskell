module Crib04 where


----------------------------------
-- lists by enumeration

x1 = [1,9,265]
 

x2 = [3 .. 7]       -- [3,4,5,6,7]
 

x3 = [3,5 .. 10]    -- [3,5,7,9]
 

x4 = [10,9 .. 5]    -- [10,9,8,7,6,5]
 

x5 = [100,90 .. 60] -- [100,90,80,70,60]
 

---------------------------------
-- lists by comprehension
 
x6 = [ x+1 | x <- x1] -- [2,10,266]
 
 
x7 = [ (i,j) | i <- x1, j <- [2,4] ]
  -- [(1,2),(1,4),(9,2),(9,4),(265,2),(265,4)]
    

x8 = [ n | n <- [1,2,3,4,5,6], even n] 
  -- [2,4,6]


x9 = [ (i,j) | i <- x2, even i, j <- x4, odd j]
  -- [(4,9),(4,7),(4,5),(6,9),(6,7),(6,5)]

  
x10 = [ [ x+y | x <- [1,2]] | y <- [9,10] ]  
 -- [[10,11],[11,12]]
 
 
x11 = [ length x | x <- x10 ] -- [2,2]
 
 
---------------------------------------
-- infinite lists

forever = [5,10 ..] -- [5,10,15,20, ...]

ones = repeat 1  -- [1,1,1,1,1, ...]

pairs = cycle [1,2] -- [1,2,1,2,1,2, ...]

f x = x+1

from3 = iterate f 3   
  -- [3, f 3, f(f 3), f(f(f 3)), ...] ==
  -- [3,4,5,6,...]
