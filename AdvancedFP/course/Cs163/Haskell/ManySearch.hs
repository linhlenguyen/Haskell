module ManySearch where

import ArrCommands

----------------------------------------------------
-- Sequential serach of a list.

seqListSearch:: Eq key => key -> [(key,value)] -> Maybe value
seqListSearch key [] = Nothing
seqListSearch key ((k,v):xs) | key==k = Just v
                             | True = seqListSearch key xs


-----------------------------------------------------
-- Sequential search of an array. Uses the item by item
-- array processing design pattern.


seqArraySearch:: Eq a => a -> Array(a,b) -> IO (Maybe b)

seqArraySearch key arr = 
  do { (low,hi) <- boundsArr arr
     ; worker (low,hi) key arr
     }

worker (low,hi) key arr | low > hi = return Nothing
worker (low,hi) key arr = 
  do { (k,v) <- readArr arr low
     ; if key==k
          then return(Just v)
          else worker (low+1,hi) key arr
     }
     
--------------------------------------------------------------
-- Invariant that the array is alaways sorted by key type "a"


binArraySearch:: (a -> Ordering) -> Array(a,b) -> IO (Maybe b)
binArraySearch pred arr =   
  do { (low,hi) <- boundsArr arr
     ; divide (low,hi) pred arr
     }

-- The algorithm works by selecting a contiguous slice of the
-- array bounded by "low" and "hi". If this slice is small
-- then we have come to an exit condition. Whether or not this
-- exit condition is a success or failure depend upon the small
-- number of (key,value) pairs stored in this slice

divide:: (Int,Int) -> (a -> Ordering) -> Array(a,b) -> IO (Maybe b)
divide (low,hi) pred arr 
   | low > hi = return Nothing                    -- slice contains nothing. Must be failure
   | low == hi = do { (k,v) <- readArr arr low    -- slice contains one pair. Check it.
                    ; case pred k of
                       EQ -> return(Just v)
                       _ -> return Nothing}
   | (low + 1) == hi =                            -- slice contains 2 pairs, check both
        do { (k1,v1) <- readArr arr low
           ; (k2,v2) <- readArr arr hi
           ; case (pred k1, pred k2) of 
              (EQ,_) -> return(Just v1)
              (_,EQ) -> return(Just v2)
              (_,_) -> return Nothing
          }
divide (low,hi) pred arr =                       -- slice is large
  do { let mid = ((hi - low) `div` 2) + low      -- choose a midpoint
     ; (k,v) <- readArr arr mid                  -- get pair stored at midpoint
     ; case pred k of                            -- compare to search key
        EQ -> return(Just v)                     -- if its equal exit with success
        LT -> divide (low,mid -1) pred arr       -- other wise compute a new slice
        GT -> divide (mid+1,hi) pred arr
     }

-- Some simple data to test with

dat :: [(Int,Char)]
dat = zip [1..20] ['a' ..]

-- a simple test in the Command language

test1 =
  do { arr <- newListArr (101,120) dat
     ; printArr arr
     ; let find x = (\ elem -> compare x elem)
     ; a1 <- binArraySearch (find  4) arr
     ; print a1
     ; a2 <- binArraySearch (find  42) arr
     ; print a2
     }
--------------------------------------------------------------------
-- sequentially linked structures can't support binary
-- search because accessing the the mid point can take time proportional
-- to the length of the list. So we need doubly linked, or tree like data

data BinSearchTree a 
   = Empty 
   | Branch (BinSearchTree a) a (BinSearchTree a)
 deriving Show

-- Note to use a tree in a binary search the tree must maintain an
-- invariant.  For a tree with shape (Branch left x right)
-- All elements in left must be < x, and all elements in right
-- must be >= x


-- insertTree adds a (key,value) pair to a tree that maintains
-- the invariant, and returns a new tree that also maintains
-- the invariant.

insertTree :: (Eq key,Ord key) => (key,value) -> BinSearchTree (key,value) ->  BinSearchTree (key,value)
insertTree (k,v) Empty = Branch Empty (k,v) Empty
insertTree (k,v) (Branch left (key,satelite) right) 
  | k>=key =  Branch left (key,satelite) (insertTree (k,v) right)
  | k < key =  Branch (insertTree (k,v) left) (key,satelite) right

-- testf creates a test tree that maintains the invariant from
-- a list of keys. The value is always one less than the key.

testf [] = Empty
testf (x:xs) = insertTree (x,x-1) (testf xs)
  
-- 2 sample trees maintaining the invariant

t1 = Branch (Branch Empty 3 Empty)  4 (Branch (Branch Empty 5 Empty) 7 Empty) 
t2 = testf [8,4,17,4,25,14,17,89,3]

-- The binary search algorithm. The tree must maintain the invariant
-- for the algorithm to work.

binTreeSearch:: (Eq key,Ord key) => key -> BinSearchTree (key,value) -> Maybe value
binTreeSearch elem Empty = Nothing
binTreeSearch elem (Branch left (k,v) right) 
 | elem == k = Just v
 | elem < k = binTreeSearch elem left
 | elem > k = binTreeSearch elem right
