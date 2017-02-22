module ComparingSortingGrouping  where

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
  ,("Jim",80,Blue,"Vermont")
  ,("Robert",23,Red,"California")
  ,("Lois",32,Green,"Michigan")
  ,("Barbara",50,Red,"Oregon")
  ,("Caleb",15,Yellow,"Utah")
  ,("Vicki",24,Red,"Oregon")
  ,("David",50,Green,"Oregon")
  ,("Justin",50,Purple,"Oregon")
  ,("Andrew",29,Red,"Oregon")
  ]
  
name   (nm,ag,clr,st) = nm  
age   (nm,ag,clr,st)  =  ag
color (nm,ag,clr,st)  = clr  
state (nm,ag,clr,st)  = st  

------------------------------------
-- the names of all people who live in Oregon

p1 = [ name x | x <- people, state x == "Oregon" ]

-------------------------------------------
-- How many people live in California

p2 = length [ 1 | x <- people, state x =="California" ]


-------------------------------------------------
-- the list of ages of people who live in Utah

p3 = [ age x | x <- people, state x == "Utah"]


----------------------------------------------
-- The names of all people in the survey
-- in alphabetical order

p4 = sort [ name x | x <- people]


----------------------------------------------------
-- The names and color of all people, sorted by color

p5:: [(String,Color)]
p5 = sortBy f [(name x, color x) | x <- people]
  where f (x,y) (m,n) = compare y n

--------------------------------------------------------
-- The color and the count of all those with that color

p6:: [(Color,Int)]
p6 =  [ get x | x <- groupBy g p5]
 where get x = (snd (head x), length x)
       g (x,y) (m,n) = y==n

-----------------------------------------------------
-- The color and a list of all names with that color

p7:: [(Color,[String])]
p7 = [ get x | x <- groupBy g p5]
  where get x = (snd (head x), [ n | (n,c) <-  x])
        g (x,y) (m,n) = y==n


----------------------------------------------
-- The names and ages of all those who live in Oregon
-- grouped by age.

p8:: [[(String,Integer)]]
p8 = ys
  where xs = [ (name x,age x) | x <- people, state x == "Oregon"]
        ys = groupBy g (sortBy f xs)
        f (x,y) (m,n) = compare y n
        g (x,y) (m,n) = y==n
