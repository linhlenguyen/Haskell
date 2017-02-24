
module DemoSelfRating where

import PriorLearn
import List(sort,group,sortBy,elem,groupBy)
import Excell

double :: Int -> Double
double = fromIntegral

mapGroup:: Ord key => ([payload] -> ans) -> [(key,payload)] -> [(key,ans)]
mapGroup f xs = [ g x | x <- grouped ]
  where comp (k1,p1) (k2,p2) = compare k1 k2
        eq (k1,p1) (k2,p2) = k1 == k2
        grouped = groupBy eq (sortBy comp xs)
        g xs = (fst(head xs),f [ snd y | y <- xs])        
        
------------------------------------------------------------------------        

temp1 = [ (major41 x,(rate_QuantReasoning100 x, rate_DiscussSocialProblems102 x)) 
        | x <- priorLearnData 
        , not(major41 x `elem` ["Undecided","Did not answer",""])
        ]

temp2 = mapGroup f temp1
  where f xs = (count,quantAverage,socialAverage)
           where count = length xs
                 quantAverage  = average[quant | (quant,soc) <- xs, quant <= 5]
                 socialAverage = average[soc   | (quant,soc) <- xs, quant <= 5]
        
average xs = double (sum xs) / double(length xs)
     
temp3 =  [ x | x <- temp2, largeEnough x]
  where largeEnough (maj,(count,quant,social)) = count >= 10

table = (blankRow 1 `beside` row ["Quantitative self rating", "Social self rating"]) 
        `above`
        col (sortBy test (map project temp3))
  where project (maj,(count,quant,social)) = (maj,quant,social)
        test (m1,q1,s1) (m2,q2,s2) = compare q2 q1

main = export "TableSelfRating" table


--------------------------------

push x = putStrLn (show x ++ "\n")

view1 = take 5 temp1
view2 = mapM push (take 5 (mapGroup id temp1))
view3 = mapM push (take 5 temp2)
