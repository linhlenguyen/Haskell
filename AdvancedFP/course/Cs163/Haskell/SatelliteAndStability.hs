module SatelliteAndStability  where

l1 = [("Tim",16)
     ,("Mary",24)
     ,("John",45)
     ,("Joan",26)
     ,("Bill",26)
     ,("Ann",24)
     ,("Ryan",52)
     ,("John",8)
     ,("Barbara",31)
     ]
     
-- If we sort l1 we get
-- [("Ann",24),("Barbara",31),("Bill",26),("Joan",26),("John",8)
-- ,("John",45),("Mary",24),("Ryan",52),("Tim",16)]
--
-- What if we wanted to sort by age?
-- This problem is known as the sattellite data problem
-- elements are broken into "keys" and "Satellite data"
-- In this example the key is the age, and the data is the satellite data
-- To handle this we need to modify our sorting algorithm.

------------------------------------------------------
-- 1) The old way. Do not consider satellite data

insert1 :: (Ord t) => t -> [t] -> [t]
insert1 elem [] = [elem]
insert1 elem (item : xs) | elem == item  = item : elem : xs
insert1 elem (item : xs) | elem < item   = elem : item : xs
insert1 elem (item : xs) | elem > item   = item: (insert1 elem xs)


sort1 :: Ord a => [a] -> [a]
sort1 [] = []
sort1 (x:xs) =  insert1 x (sort1 xs)

--------------------------------------------------------------
-- 2) Pass a function that extracts the key from every element

insert2 :: (Ord a) => (t -> a) -> t -> [t] -> [t]
insert2 keyf elem [] = [elem]
insert2 keyf elem (item : xs) 
  | keyf elem == keyf item  = item : elem : xs
  | keyf elem <  keyf item  = elem : item : xs
  | keyf elem >  keyf item  = item: (insert2 keyf elem xs)


sort2 :: (Ord a1) => (a -> a1) -> [a] -> [a]
sort2 keyf [] = []
sort2 keyf (x:xs) =  insert2 keyf x (sort2 keyf xs)

-- Now if we sort l1 with appropriate key function
-- *SatelliteAndStability> sort2 snd l1
-- [("John",8),("Tim",16),("Ann",24),("Mary",24),("Bill",26)
-- ,("Joan",26),("Barbara",31),("John",45),("Ryan",52)]

--------------------------------------------------------------
-- 3) Rather than pass  a function that extracts the key from 
--    every element, pass a generic comparison function

insert3 :: (t -> t -> Ordering) -> t -> [t] -> [t]
insert3 cmp elem [] = [elem]
insert3 cmp elem (item : xs) = 
   case cmp elem item of
     LT -> elem : item : xs
     EQ -> item : elem : xs
     GT -> item: (insert3 cmp elem xs)
 
sort3 :: (a -> a -> Ordering) -> [a] -> [a]
sort3 cmp [] = []
sort3 cmp (x:xs) =  insert3 cmp x (sort3 cmp xs)

ageCmp (n1,a1) (n2,a2) = compare a1 a2
test3 = sort3 ageCmp l1
-- [("John",8),("Tim",16),("Ann",24),("Mary",24),("Bill",26)
-- ,("Joan",26),("Barbara",31),("John",45),("Ryan",52)]

-----------------------------------------------------------
-- Stability
-- A sort is said to be stable, if when sorting by key, the
-- satellite data with equal keys, remains in the same relative
-- order. For example, when sorting the list l1 by age, we expect
-- people with the same age to remain in the same order.

-- l1 = [("Tim",16),("Mary",24),("John",45),("Joan",26),("Bill",26)
--      ,("Ann",24),("Ryan",52),("John",8),("Barbara",31)]
-- Mary and Ann have the same age
-- so do Joan and Bill.

-- 1) Is the insertion sort we defined above stable?
-- 2) If not, can it be altered to make it stable?