module HeapSort where

import Heaps
import ArrCommands

------------------------------------------------------------------
-- This sort is based upon the left-tree based heap implementation.
-- It works by ading every element of the input list to an empty heap.
-- Then it deletes the minimum element n-times to create a sorted list.

heapSort:: Ord a => [a] -> [a] 
heapSort xs = fromHeap (toHeap xs)

fromHeap heap 
  | isEmpty heap = []
  | True = case (findMin heap,deleteMin heap) of
            (Just elem,Just heap2) -> elem :(fromHeap heap2)
        
toHeap [] = empty
toHeap (x:xs) = insert x (toHeap xs)

-------------------------------------------
-- Here is an animation of how the sort works

{-
First build a heap from the input list

toHeap [3,7,12,4,98,65,3,4,12,32]

            +-----------3
       +----3-------+
     +-4-+        +-4-+
  +-65   12    +-12   7
 98           32

___________________________________________
The successively, delete the min and store
it in a list (see [3] below).

       +----3-------+
     +-4-+        +-4-+
  +-65   12    +-12   7
 98           32


[3]
____________________________

       +------4----+
     +-4----+    +-65
  +-12    +-7   98
 32      12


[3,3]
____________________________

    +-------4----+
  +-7----+     +-12
 12    +-65   32
      98


[4,3,3]
____________________________

     +-------7-+
  +-12----+    12
 32     +-65
       98


[4,4,3,3]
____________________________

  +-12-------+
 32        +-12
        +-65
    98


[7,4,4,3,3]
____________________________

     +-12-+
  +-65    32
 98


[12,7,4,4,3,3]
____________________________

     +-32
  +-65
 98


[12,12,7,4,4,3,3]
____________________________

  +-65
 98


[32,12,12,7,4,4,3,3]
____________________________

 98


[65,32,12,12,7,4,4,3,3]
____________________________



[98,65,32,12,12,7,4,4,3,3]
____________________________
 
-} 

-----------------------------------------------------------
-- code to create the animation above using leftist Heaps

display h _ | isEmpty h = return ()
display h xs =
  case (findMin h,deleteMin h) of
    (Just elem,Just heap2) ->
        do { putStrLn (show heap2)
           ; putStrLn (show (elem:xs))
           ; putStrLn "________________________________________"
           ; display heap2 (elem:xs) }
           
           
-----------------------------------------------------------
-- This sort is based upon the heap implementation that
-- embeds trees with the heap property in an array.
-- It builds a Heap that uses the input array, by 
-- using reHeapUp on each element to bubble it up so that
-- every element meets the heap property. It then deletes
-- the Max element n-times. Since deleteMax leaves the
-- max in the slot that used to be the last slot for the
-- heap, the array ends up with the largest element in the
-- last slot, the second largest in the second to last slot, etc.
-- So the array ends up in increasing order. So its sorted.

heapSort2:: (Show a,Ord a) => Array a -> IO()
heapSort2 arr = 
  do { (lo,hi) <- boundsArr arr
     ; let heap = Heap lo arr
     ; if lo /= 0
          then error ("Can only sort zero based arrays using heap sort.")
          else return()
     ; heap2 <- toHeap2 (lo+1,hi) heap
     ; fromHeap2 (lo+1,hi) heap2
     ; return()
     }

toHeap2 (lo,hi) heap | lo > hi = return heap
toHeap2 (lo,hi) (heap@(Heap _ arr)) = 
  do { reHeapUp lo heap
     ; toHeap2 (lo+1,hi) (Heap lo arr) }

fromHeap2 (lo,hi) heap | lo > hi = return heap
fromHeap2 (lo,hi) heap = 
 do { heap2 <- deleteMax heap     
      -- DeleteMax leaves max in what was the last slot
    ; fromHeap2 (lo+1,hi) heap2 }


main = do { let input = [3,7,12,4,98,65,3,4,12,32]
          ; arr <- newListArr (0,9) input
          ; heapSort2 arr
          ; output <- toListArr arr
          ; putStrLn ("Input  = "++show input)
          ; putStrLn ("Output = "++show output)
          }
          
---------------------------------------------------------
-- Here is an animation of how the sort works on arrays

{-
The heap starts with a single node, which is the first
element of the array. A Heap of size one always meets
the heap property.

3

 0 1  2 3  4  5 6 7  8  9    index
[3,7,12,4,98,65,3,4,12,32]   values  last = 0

________________________________________
Then, trickle up each element 7, 12, 4, etc.
until the whole array is a heap.

         +------98---+
    +---32---+     +-65-+
 +-12-+    +-12    7    3
 3    4    4


  0  1  2  3  4 5 6 7 8 9    index
[98,32,65,12,12,7,3,3,4,4]   values  last = 9

________________________________________
Then delete the max, note how 98 ends up
in the last slot of the array

         +----65---+
    +---32-+     +-7-+
 +-12-+    12    4   3
 3    4


  0  1 2  3  4 5 6 7 8  9    index
[65,32,7,12,12,4,3,3,4,98]   values  last = 8

________________________________________
delete 65

      +----32---+
   +-12-+     +-7-+
 +-4    12    4   3
 3


  0  1 2 3  4 5 6 7  8  9    index
[32,12,7,4,12,4,3,3,65,98]   values  last = 7

________________________________________
delete 32

    +---12---+
 +-12-+    +-7-+
 4    3    4   3


  0  1 2 3 4 5 6  7  8  9    index
[12,12,7,4,3,4,3,32,65,98]   values  last = 6

________________________________________
delete 12

   +---12---+
 +-4-+    +-7
 3   3    4


  0 1 2 3 4 5  6  7  8  9    index
[12,4,7,3,3,4,12,32,65,98]   values  last = 5

________________________________________
delete 12

   +---7-+
 +-4-+   4
 3   3


 0 1 2 3 4  5  6  7  8  9    index
[7,4,4,3,3,12,12,32,65,98]   values  last = 4

________________________________________
delete 7 

   +-4-+
 +-3   4
 3


 0 1 2 3 4  5  6  7  8  9    index
[4,3,4,3,7,12,12,32,65,98]   values  last = 3

________________________________________
delete 4 

 +-4-+
 3   3


 0 1 2 3 4  5  6  7  8  9    index
[4,3,3,4,7,12,12,32,65,98]   values  last = 2

________________________________________
delete 4

 +-3
 3


 0 1 2 3 4  5  6  7  8  9    index
[3,3,4,4,7,12,12,32,65,98]   values  last = 1

________________________________________
delete 3

 3


 0 1 2 3 4  5  6  7  8  9    index
[3,3,4,4,7,12,12,32,65,98]   values  last = 0

________________________________________

-}