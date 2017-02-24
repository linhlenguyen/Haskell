module CombineHUnitQuickCheck where

import Test.HUnit  -- (Test(TestCase,TestList),assertEqual,runTestTT)
import Test.QuickCheck
import Test.QuickCheck.Test(isSuccess)

-- Use this like TestCase with a label
-- can be applied to any QuickCheck property
-- See "t4" below
quickCase :: Test.QuickCheck.Testable prop => [Char] -> prop -> Test
quickCase label prop = TestLabel name (TestCase comp)
   where comp = do { ans <- quickCheckWithResult arg prop
                   ; if isSuccess ans
                        then do { putStrLn (name++"\n   "++output ans) ; return () }
                        else assertFailure (output ans) }
         arg = stdArgs{chatty = False,maxSuccess=150}
         name = "\nquickCase: "++label
  
addOne :: [Int] -> [Int]
addOne xs = [(x+1) | x <- xs]

t1 = TestCase (assertEqual "addOne []" (addOne []) [])
t2 = TestCase (assertEqual "addOne [2,7]" (addOne [2,7]) [3,8])
t3 = TestCase (assertEqual "addOne [4,-3,2]" (addOne [4,-3,2]) [5,-2,3])
t4 = quickCase "addOne sum prop" prop1

go = runTestTT $ TestList[t1,t2,t3,t4]

prop1 xs = -- collect (length xs) 
          (sum (addOne xs) == sum xs + length xs)
