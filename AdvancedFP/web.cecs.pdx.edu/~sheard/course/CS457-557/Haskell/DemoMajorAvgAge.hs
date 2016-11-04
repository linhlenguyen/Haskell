
module DemoMajors where

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
-----------------------------------------------------------

majorAge = [ (major41 x,age109 x) | x <- priorLearnData, age109 x /= 101]

average xs = double(sum xs) / double(length xs)
                      
answer = mapGroup (\ x -> (average x,length x)) majorAge
  
descending = sortBy test answer
  where test (_,(a1,c1)) (_,(a2,c2)) = compare a1 a2

maxCount = (maximum [ cnt | (mj,(avg,cnt)) <- descending ])  
maxAvgAge = (maximum [ avg | (mj,(avg,cnt)) <- descending ])


normFactor = double maxCount / maxAvgAge

normalized = [ (maj,(avg,double count / normFactor)) 
             | (maj,(avg,count)) <- descending
             , count >= 5
             , maj /= "" 
             ]
             
table = (blankRow 1 `beside` row ["Avg Age","Norm. Major Count"]) `above`
        col normalized
  
main = export "DemoMajorsAge" table
