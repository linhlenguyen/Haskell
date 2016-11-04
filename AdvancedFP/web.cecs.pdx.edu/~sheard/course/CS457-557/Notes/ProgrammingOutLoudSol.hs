module ProgrammingOutLoud  where

import Test.HUnit -- imports TestCase,assertEqual,TestList,runTestTT and others
import Data.Char(ord)

{-

"345" ( ['3','4','5'] )
[3,4,5]
[(3,100),(4,10),(5,1)]
[300,40,5]
345

-}

stringToInt x1 = x8
 where -- x1 = "345"
       x2 = map ord x1
       minus x = toInteger(x - ord '0')
       x3 = map minus x2
       x4 = take (length x1) [0..]
       x5 = reverse [ 10 ^ i | i <- x4 ]
       x6 = zip x3 x5
       mult (x,y) = x * y
       x7 = map mult x6
       x8 = sum x7 





-------------------------------------------------
test1 = TestCase (assertEqual "'345' =?= 345" (stringToInt "345") 345)
test2 = TestCase (assertEqual "'0' =?= 0" (stringToInt "0") 0)
test3 = TestCase (assertEqual "'' =?= 0" (stringToInt "") 0)

testall = runTestTT (TestList [test1,test2,test3])                              
 