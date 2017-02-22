module DesignRecipeExample where

import Test.HUnit(Test(..),assertEqual,runTestTT)
import List(sort)
import Test.QuickCheck
import ArrCommands


-- 1) Understand the problem
-- 3) Write a contract (type) for each function
-- 3) Create a set of examples
-- 4) Create a body for each function
--       the structure of the body mirrors 
--       the structure of the input data
-- 5) Test your program

--------------------------------------------------------

-- 1) Understand the problem
-- The problem involves inserting an element into a 
-- a sorted sequence in a manner that the new sequence is also sorted.
-- The sequence could be implemented as an mutable array or a list.
--
-- The mutable array implementation would require the use of commands
-- as the function would change the state of the array by moving 
-- elements around.
--
-- A list based implementation could be implemented as a pure
-- computation by creating a new list with the desired property
-- by copying (or sharing pointers) with the old list.

------------------------------------------------------
-- 2) Create a contract

insert:: Ord a => a -> [a] -> [a]
-- insert elem list = undefined

--------------------------------------------------------
-- 3) Create a set of examples
ex1 = assertEqual "empty"     (insert 3 [])        [3]
ex2 = assertEqual "first"     (insert 1 [4,5,6])   [1,4,5,6]
ex3 = assertEqual "middle"    (insert 4 [1,3,6])   [1,3,4,6]
ex4 = assertEqual "last"      (insert 6 [5])       [5,6]
ex5 = assertEqual "duplicate" (insert 4 [1,4,6,8]) [1,4,4,6,8]

------------------------------------------------------
-- 4) Create a body for each function
--       the structure of the body mirrors 
--       the structure of the input data

-- The structure of the list is either 
-- a) empty, or 
-- b) has at least one item
--
-- insert elem [] = undefined
-- insert elem (x:xs) = undefined
--
-- if the list has at least one item then three cases apply
--   1) the element to insert is equal to the item
--   2) the element to insert is less than the item
--   3) the element to insert is greater than the item
-- insert elem [] = [elem]
-- insert elem (item : xs) | elem == item = undefined
-- insert elem (item : xs) | elem < item = undefined
-- insert elem (item : xs) | elem > item = undefined


insert elem [] = [elem]
insert elem (item : xs) | elem == item  = elem : item : xs
insert elem (item : xs) | elem < item   = elem : item : xs
insert elem (item : xs) | elem > item   = item: (insert elem xs)

--------------------------------------------------------
-- 5) Test your program

listTests = 
   TestList [ TestCase ex1 
            , TestCase ex2
            , TestCase ex3
            , TestCase ex4
            , TestCase ex5 ]

testList = runTestTT listTests

--------------------------------------
-- Write a function that tests if a list is sorted

-- Understand: all elements occur in the list in ascending order

-- Contract
sorted:: Ord t => [t] -> Bool

-- Examples

ex6 = assertEqual "empty is sorted"  (sorted ([]::[Int])) True
ex7 = assertEqual "single is sorted" (sorted [3])         True
ex8 = assertEqual "down is not"      (sorted [3,2,1])     False
ex9 = assertEqual "up is sorted"     (sorted [3,7,9])     True

-- Body 3 cases
sorted [] = True
sorted [x] = True
sorted (x:y:more) = (x <= y) && (sorted (y:more))

-- Tests
sortedTests = 
   TestList [ TestCase ex6 
            , TestCase ex7
            , TestCase ex8
            , TestCase ex9 ]

testSorted = runTestTT sortedTests

-----------------------------------------
-- A random test on insert

prop_insert_sorted_sorted :: Int -> [Int] -> Bool
prop_insert_sorted_sorted x xs = sorted(insert x (sort xs))

randomTest = quickCheck prop_insert_sorted_sorted

-- ========================================================
-- Recipe for an array based implementation

-- 1) Understand the problem
-----------------------------------------------------
-- Given Array(1,7)[99,105,245,_,_,_,_]
-- Staring from the right (i.e. the left most empty slot)
-- move elements to right to find the correct location
-- insertC 3 100 [99,105,245,_,_,_,_] -->
-- insertC 2 100 [99,105,_,245,_,_,_] -->
-- insertC 1 100 [99,_,105,245,_,_,_] --> [99,100,105,245,_,_,_]

-- 2) Write a contract

insertC :: Ord a => Int -> a -> (Array a) -> IO ()
-- insertC emptySlot elem array = undefined

insertArr:: Ord a => Int -> Int -> a -> Array a -> IO ()
-- insertArr lowBound emptySlot elem array = undefined


-- 3) Create a set of examples


-- 4) Create bodies - the structure of the body mimics 
--                    the structure of the the input data

insertArr lowBound emptySlot elem array =
  if emptySlot==lowBound
     then writeArr array emptySlot elem
     else do { v <- readArr array (emptySlot - 1)
             ; if v < elem
                  then writeArr array emptySlot elem
                  else do { writeArr array emptySlot v -- slide it to the right
                          ; insertArr lowBound (emptySlot-1) elem array } }

insertC nextSlot elem array = 
  do { (low,high) <- boundsArr array
     ; insertArr low nextSlot elem array
     }

main =
 do { a <- newListArr (1,10) [] 
    ; insertC 1 5 a
    ; printSomeArr 1 a
    ; insertC 2 8 a
    ; printSomeArr 2 a
    ; insertC 3 2 a
    ; printSomeArr 3 a
    ; insertC 4	6 a
    ; printSomeArr 4 a
    ; putStrLn "DONE"
    }
