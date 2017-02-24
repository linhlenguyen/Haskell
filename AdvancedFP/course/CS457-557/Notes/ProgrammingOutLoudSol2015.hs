module ProgrammingOutLoud  where
import Test.HUnit -- imports TestCase,assertEqual,TestList,runTestTT and others
import Data.Char(ord)

{-
"345" ( ['3','4','5'] )
[3,4,5]
[(5,1),(4,10),(3,100)]
[5,40,300]
345

-}

stringToInt x1 = (sum .
                  map mult .
                  powers . 
                  map decimal) x1
  where mult (x,y) = x * y
        powers xs =  zip [ 10^i | i <- [0..]] 
                         (reverse xs)
        decimal x 
         | x<='9' && x >='0' = toInteger (ord x - ord '0')
         | True = error ("Bad char in decimal "++[x])

 




-------------------------------------------------
-- test1 = TestCase (assertEqual "'345' =?= 345" (stringToInt "345") 345)
 
 
-- testall = runTestTT (TestList [test1,test2,test3])                              
 