module DemoMajors where

import PriorLearn
import List(sort,group,sortBy,elem,groupBy)
import Excell

pld = priorLearnData

majorGenderAll = [ (major41 x,gender110 x) | x <- pld]

majorGender = [ (m,g) | (m,g) <- majorGenderAll
                      , not(m `elem` ["Undecided","Did not answer",""])
                      , not(g `elem` [101,99])
                      ]

double :: Int -> Double
double = fromIntegral

mapGroup:: Ord key => ([payload] -> ans) -> [(key,payload)] -> [(key,ans)]
mapGroup f xs = [ g x | x <- grouped ]
  where comp (k1,p1) (k2,p2) = compare k1 k2
        eq (k1,p1) (k2,p2) = k1 == k2
        grouped = groupBy eq (sortBy comp xs)
        g xs = (fst(head xs),f [ snd y | y <- xs])                      
                      
answer = mapGroup count majorGender
  where count x = (length[ z | z <- x, z==1],length [ z | z<- x, z==2])
  
descending = sortBy test answer
  where test (_,(m,f)) (_,(b,g)) = compare  (b+g) (m+f)

table = (blankRow 1 `beside` row ["Male","Female"]) `above`
        col descending
  
main = export "DemoMajors" table

