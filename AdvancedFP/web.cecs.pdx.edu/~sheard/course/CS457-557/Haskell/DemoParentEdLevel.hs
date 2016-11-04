module DemoParentEdLevel where

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

--------------------------------------------------------        

fatherEdchildGrad = [ (highestEducationFather112 x
                      , howLikelyAreYouToCompleteBachelorsDegreeAtPsu3 x)
                    | x <- priorLearnData ]
    
labeledPairs = mapGroup label fatherEdchildGrad    

percentage p xs = round ( ( total  / count ) * 100.0 )
   where total = double (length  [y | y <- xs, p y])
         count = double (length xs)

labels = ["Child is very likely"      ,"Child is somewhat likely"
         ,"Child is somewhat unlikely","Child is very unlikely"]

label xs = [percentage (==4) xs   -- Child is very likely
           ,percentage (==3) xs   -- Child is somewhat likely
           ,percentage (==2) xs   -- Child is somewhat unlikely
           ,percentage (==1) xs]  -- Child is very unlikely

labeledPercent = [ (edLevel x,y) | (x,y) <- labeledPairs ]

edLevel 1 = "Father did not graduate from highschool"
edLevel 2 = "Father is high school graduate or has GED"
edLevel 3 = "Father has some college including 2 year degree"
edLevel 4 = "Father has bachelors degree"
edLevel 5 = "Father has graduate degree (MS,MD,PhD,JD, etc)"
edLevel _ = "?"
          
tab = (blankRow 1 `beside` row labels) `above` 
      stack (take 5 [ (row [father] `beside` row percents) 
                    | (father,percents) <- labeledPercent ])
  
main = export "TableParentEdLevel" tab


view1 = take 10 fatherEdchildGrad
view2 = take 10 labeledPairs
view3 = take 5 labeledPercent