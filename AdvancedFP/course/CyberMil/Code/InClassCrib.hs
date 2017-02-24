module InClassCrib where

import Excell
import PriorLearn
import List(sort,group,sortBy,groupBy)

------------------------------------------------------
-- A quick example to illustrate why sorting is
-- necessary before and grouping

x1 = [2,567,34,2,87,4,2,34,567,4,2]

view1 = group x1
view2 = group (sort x1)

------------------------------------------------------
-- A running example using gender, age, and major

x2 = [ (gender110 x,(major41 x, age109 x)) | x <- priorLearnData]

-- A small subset of the data just to look at.

view3 = take 30 (drop 1300 x2)

-- Create a function so we can sort by age (gender,(major,age))
test3rd (gender,(major,age)) (gender2,(major2,age2)) = compare age age2

-- Look at the result of sorting by age
view4 = sortBy test3rd view3

-- Create a function so we can group by age (gender,(major,age))
equal3rd (gender,(major,age)) (gender2,(major2,age2)) = age == age2

-- Look at the result of grouping by age
view5 = groupBy equal3rd view4

-------------------------------------------------------------------
-- Capture this pattern

mapGroup f list = 
   [ compress f elem | elem <- groupBy equalf (sortBy testf list) ]

equalf (x1,y1)  (x2,y2) = x1==x2
testf (x1,y1) (x2,y2) = compare x1 x2

------------------------------------------------------
-- Look at just one of the groups
-- Study its structure.

first = head (mapGroup id view3)

------------------------------------------
-- Each group should be compressed getting
-- rid of the keys which all the same within
-- a group. Keep only the first key::  fst(head list)
-- and process the list of payloads with: whatToDo

compress whatToDo list = 
    (fst(head list), whatToDo [ y | (x,y) <- list] )

---------------------------------------------------------------
justMajor list = [ major | (major,age) <- list ]

y1 = mapGroup myAvg [ (major41 x, age109 x) | x <- priorLearnData]

myAvg xs = avg [ fromIntegral x | x <- xs, x /= 101 ]

avg xs = sum xs / (fromIntegral (length xs))