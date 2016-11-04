module ManySortsOrganized  where

import ArrCommands

---------------------------------------------
-- swapping, conditional and unconditional

testAndSwap:: (a -> a -> Ordering) -> Array a -> (Int, Int) -> IO ()
testAndSwap p a (i,j) =
  do { tempi <- readArr a i
     ; tempj <- readArr a j
     ; case p tempi tempj of
	 GT -> do { writeArr a i tempj; writeArr a j tempi}
	 LT -> return ()
	 EQ -> return ()
      }
      
swap arr (i,j) = testAndSwap (\ x y -> GT) arr (i,j)      
	
--------------------------------------------------------
-- permute, given a predicate, and a list of index pairs
-- to conditionally swap, swap those pairs that meet the condition

permute:: (a -> a -> Ordering) ->  Array a -> [(Int,Int)] -> IO()
permute pred arr [] = return ()
permute pred arr ((i,j):xs) = 
  do { testAndSwap pred arr (i,j)
     ; permute pred arr xs }

------------------------------------------------

n `downTo` m = [n, n-1 .. m]
exchangeIndices (low,hi) = [(i,j) | i <- [low .. (hi-1)], j <- [i+1 .. hi] ]
insertIndices (low,hi) = [(i,i+1) | j <- (hi-1) `downTo` low, i <- (hi-j) `downTo` low ]
bubbleIndices (low,hi) = [(i,i+1) | max <- (hi-1) `downTo` low, i <- [low ..max]]

------------------------------------------------
-- Now sorting is all about permuting with the right
-- set of indices

exchangeSort1 pred array =
 do { (l,h) <- boundsArr array
    ; permute pred array (exchangeIndices (l,h)) }
    
bubbleSort1 pred array =
 do { (l,h) <- boundsArr array
    ; permute pred array (bubbleIndices (l,h)) }
    
insertSort1 pred array =
 do { (l,h) <- boundsArr array
    ; permute pred array (insertIndices (l,h)) } 

-- There is a pattern here, can we name it?

permuteArr:: 
  (a -> a -> Ordering) -> ((Int, Int) -> [(Int, Int)]) -> Array a -> IO ()
permuteArr pred indices array =  
 do { (l,h) <- boundsArr array
    ; permute pred array (indices (l,h)) }


-------------------------------------------------
-- Now the big win 

exchangeSort pred arr = permuteArr pred exchangeIndices arr
bubbleSort   pred arr = permuteArr pred bubbleIndices arr
insertSort   pred arr = permuteArr pred insertIndices arr

reverseArr arr = permuteArr always indices arr
  where always x y = GT
        indices (low,hi) = take (div (hi - low) 2)
                                (zip [low .. hi] (hi `downTo` low))
        
     


    