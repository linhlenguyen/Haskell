-- <PRE>
module HUnitDemo where

import Test.HUnit(Test(TestCase,TestList),assertEqual,runTestTT)



-- 1) write a type for the function. Make its body undefined
len:: [ a ] -> Int
len x = undefined


-- 2) make some examples, and express them as HUnit assertions
-- len [] --> 0
-- len [1,1,1,1] --> 4
-- len [1,2] --> 2

ex1 = assertEqual "empty" (len [])  0
ex2 = assertEqual "all the same" (len [1,1,1,1]) 4
ex3 = assertEqual "two" (len [1,2]) 2

-- 3) Collect all the tests together
-- By convention we use the name tests
-- to collect all the Unit-tests in one file together.


lenTest = TestList[TestCase ex1, TestCase ex2, TestCase ex3]
tests = runTestTT lenTest

-- 4) Then fill in the definition until all tests are passed!

---------------------------------------------------------
-- Now try it for the sum function

-- 1) write a type for the function. Make its body undefined

-- 2) make some examples, and express them as HUnit assertions

-- 3) Collect all the tests together

-- 4) Then fill in the definition until all tests are passed!


-- </PRE> 