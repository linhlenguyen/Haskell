module CountingGrouping where


import List(sort,sortBy,group,GroupBy)


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


-- Use the library function sort sortBy

-- Use the library function group groupBy


count eqf select xs = map select (groupBy (lift eqf) (sortBy eqf xs))
  where lift f x y = f x y == EQ

