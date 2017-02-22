module HW3Review where

import Test.HUnit

find:: Eq key => key -> [(key,value)] -> Maybe value
find key [] = Nothing
-- find key ((k,v):xs) = if key==k then Just v else find key xs
find key ((k,v):xs) | key==k = Just v
                    | True = find key xs
--find key ((k,v):xs) | key==k = Just v
--find key ((k,v):xs)          = find key xs


x = [(5,"Tim"),(3,"Tom"),(6,"Mary"),(5,"Ann")]

a1 = assertEqual "yes1" (find  6 x) (Just "Mary")
a2 = assertEqual "yes2" (find  5 x) (Just "Tim")
a3 = assertEqual "no"   (find  4 x) Nothing

asserts as = TestList [ TestCase a | a <- as]

t1 = runTestTT (asserts [a1,a2,a3])

---------------------------------------------------

data BinTree a = Empty | One a | Branch (BinTree a) (BinTree a)
  deriving Show

tree1,tree2,tree3:: BinTree Int
tree1 = Empty
tree2 = Branch Empty (One 3)
tree3 = Branch tree2 (Branch (One 6) (One 1))

flatten:: (BinTree a) -> [a]
flatten Empty = []
flatten (One x) = [x]
flatten (Branch left right) = (flatten left) ++ (flatten right)

a4 = assertEqual "tree3" (flatten tree3) [3,6,1]
t2 = runTestTT (asserts [a4])

count:: (BinTree Int) -> Int
count Empty = 0
count (One x) = x
count (Branch left right) = (count left) + (count right)

plus5a :: Int -> Int 
plus5a =  \ x -> x+5


plus5b :: Int -> Int 
plus5b = (+5)  -- (5+)

plus5c :: Int -> Int 
plus5c x = x+5

-- smallest 5 6 ----> 5