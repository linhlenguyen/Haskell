module CompareSortGroup where
import List(sort,sortBy,group,groupBy)

data Color =  Blue | Green | Orange | Purple | Red | Yellow 
  deriving (Eq,Ord,Show)

people = 
  [("Tim",24,Red,"Oregon")
  ,("Tom",36,Blue,"Ohio")
  ,("Mary",19,Yellow,"Vermont")
  ,("Zach",41,Blue,"California")
  ,("Ann",9,Purple,"Michigan")
  ,("Jane",50,Red,"Oregon")
  ,("Harry",71,Green,"Utah")
  ]
  
name   (nm,ag,clr,st) = nm  
age   (nm,ag,clr,st)  =  ag
color (nm,ag,clr,st)  = clr  
state (nm,ag,clr,st)  = st  

-- Ordering = {LT,EQ,GT} with compare

c1 = compare 3 6   -- LT
c2 = compare 9 2   -- GT
c3 = compare 5 5   -- EQ

-- Alphabetic ordering

c4 = compare "abc" "xyz"  -- LT
c5 = compare "abz" "abc"  -- GT
c6 = compare "12" "12"    -- EQ

-- Lexicographic ordering on Tuples

c7 = compare (1,2,5) (4,6,9)  -- LT
c8 = compare ("Tim",99,Red) 
             ("Tim",25,Red)   -- GT
c9 = compare 
      ("Harry",71,Green,"Utah")
      ("Harry",71,Green,"Utah") -- EQ
      
-- Sorting

c10 = sort [1,67,-4,52,8]          
-- [-4,1,8,52,67]

c11 = sort [Red,Blue,Yellow,Green] 
-- [Blue,Green,Red,Yellow]


-- Sorting Tuples, Lexicographic ordering

c12 = sort [(6,3),(3,99),(6,22),(99,0)]
-- [(3,99),(6,3),(6,22),(99,0)]


-- Other orderings and sortBy

c13 = sortBy compare [1,67,-4,52,8] 
-- [-4,1,8,52,67]

c14 = sortBy test [1,67,-4,52,8] 
  where test x y = compare y x  -- note inversion
  -- [67,52,8,1,-4]
  
-- Other orderings on tuples.

c15 = sortBy second [(6,3),(3,99),(6,22),(99,0)]
  where second (x1,y1) (x2,y2) = compare y1 y2
-- [(99,0),(6,3),(6,22),(3,99)]  

c16 = sortBy secondBackwards [(6,3),(3,99),(6,22),(99,0)]
  where secondBackwards (x1,y1) (x2,y2) = compare y2 y1  
-- [(3,99),(6,22),(6,3),(99,0)] 

-- Orderings on people

c17 = sortBy increasingAge people
  where increasingAge x y = compare (age x) (age y)
{-   [("Ann",9,Purple,"Michigan")
     ,("Mary",19,Yellow,"Vermont")
     ,("Tim",24,Red,"Oregon)
     ,("Tom",36,Blue,"Ohio")
     ,("Zach",41,Blue,"California")
     ,("Jane",50,Red,"Oregon")
     ,("Harry",71,Green,"Utah")]  -}
     
c18 = sortBy f people
  where f x y = (name x) `compare` (name y)
{-  [("Ann",9,Purple,"Michigan")
    ,("Harry",71,Green,"Utah")
    ,("Jane",50,Red,"Oregon")
    ,("Mary",19,Yellow,"Vermont")
    ,("Tim",24,Red,"Oregon")
    ,("Tom",36,Blue,"Ohio")
    ,("Zach",41,Blue,"California")] -}
    
-- Grouping groups elements already adjacent

g1 = group [1,1,4,5,5,1,4]
-- [[1,1],[4],[5,5],[1],[4]]

-- Sort then group
g2 = group (sort [1,1,4,5,5,1,4])
-- [[1,1,1],[4,4],[5,5]]

-- GroupBy and tuples

g3 = groupBy first [(3,99),(6,22),(6,3),(99,0)] 
   where first (x1,y1) (x2,y2) = x1==x2
-- [[(3,99)],[(6,22),(6,3)],[(99,0)]]

-- BUT if there are no adjacent "first elemements"
g4 = groupBy first [(99,0),(6,22),(3,99),(6,3),(99,0)] 
   where first (x1,y1) (x2,y2) = x1==x2
-- [[(99,0)],[(6,22)],[(3,99)],[(6,3)],[(99,0)]]

-- Grouping and sorting combined on tuples
g5 = groupBy f (sortBy g people)
   where f x y = state x == state y
         g x y = compare (state y) (state x)
{- [[("Mary",19,Yellow,"Vermont")]
   ,[("Harry",71,Green,"Utah")]
   ,[("Tim",24,Red,"Oregon"),("Jane",50,Red,"Oregon")]
   ,[("Tom",36,Blue,"Ohio")]
   ,[("Ann",9,Purple,"Michigan")]
   ,[("Zach",41,Blue,"California")]]  -}  
