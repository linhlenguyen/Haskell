module ProgrammingProject1 where

import PriorLearn
import List(sort,group,sortBy,elem,groupBy)
import Excell


mapGroup:: Ord key => ([payload] -> ans) -> [(key,payload)] -> [(key,ans)]
mapGroup f xs = [ g x | x <- grouped ]
  where comp (k1,p1) (k2,p2) = compare k1 k2
        eq (k1,p1) (k2,p2) = k1 == k2
        grouped = groupBy eq (sortBy comp xs)
        g xs = (fst(head xs),f [ snd y | y <- xs])   
        
-----------------------------------------------------
        


temp1 = [ x | x <- priorLearnData ]

temp2 = [ ( expectedCreditsPerTerm42 x,prevYearActivities81 x) | x <- priorLearnData]

temp3 = mapGroup id temp2

temp4 = mapGroup length temp2

temp5 = mapGroup family temp2
family x = length [ act | act <- x, act == 2]



highschool x = length [ act | act <- x, act == 6]



tempN = undefined

table:: Table
table = undefined

main = export "myfileName" table