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

p1 = undefined
-------------------------------------------
-- How many people live in California

p2 = undefined


-------------------------------------------------
-- the list of ages of people who live in Utah

p3 = undefined

----------------------------------------------
-- The names of all people in the survey
-- in alphabetical order

p4 = undefined

----------------------------------------------------
-- The names and color of all people, sorted by color

p5:: [(String,Color)]
p5 = undefined
--------------------------------------------------------
-- The color and the count of all those with that color

p6:: [(Color,Int)]
p6 =  undefined

-----------------------------------------------------
-- The color and a list of all names with that color

p7:: [(Color,[String])]
p7 = undefined


----------------------------------------------
-- The names and ages of all those who live in Oregon
-- grouped by age.

p8:: [[(String,Integer)]]
p8 = undefined
