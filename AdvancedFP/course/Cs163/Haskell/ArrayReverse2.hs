module ArrayReverse where

import ArrCommands

{- ArrCommands defines the following operaions on Arrays

boundsArr:: Array a -> IO (Int, Int)
writeArr:: Array a -> Int -> a -> IO ()
readArr:: Array a -> Int -> IO a
newArr:: (Int, Int) -> a -> IO (Array a)
newListArr:: (Int, Int) -> [a] -> IO (Array a)
toListArr :: Array a -> IO [a]
showArr:: (Show a) => Array a -> IO String
printArr: (Show a) => Array a -> IO ()
printSomeArr:: (Show a) => Int -> Array a -> IO ()
-}

-- In this exercise you will write a program to reverse the elements
-- in an array. 
-- Hints
-- 1) You will need to use commands (the "do" stmt)
-- 2) break your program into 2 passes and 3 functions
--    a) Index calculation
--    b) Swapping
--    a) Iterating over the indexes

revArray:: Array a -> IO()
revArray arr = 
  do { (low,high) <- boundsArr arr
     ; let is = (indices (low,high))
     ; iterateI arr is
     }

swap:: Array a -> (Int,Int) -> IO ()
swap a (i,j) =
  do { tempi <- readArr a i
     ; tempj <- readArr a j
     ; writeArr a i tempj
     ; writeArr a j tempi
     }

indices :: (Int,Int) -> [(Int,Int)]
indices (low,high) = take ((high - low + 1) `div` 2) 
                          (zip [low .. high] (reverse [low .. high]))

iterateI:: Array a -> [(Int,Int)] -> IO ()
iterateI a [] = return ()
iterateI a ((i,j):xs) = do { swap a (i,j); iterateI a xs }


sumArr :: Array Int -> IO Int
sumArr arr = 
  do { (low,high) <- boundsArr arr
     ; let indexes = [low .. high]
           addup [] = return 0
           addup (i:is) = do { temp <- readArr arr i
                             ; rest <- addup is
                             ; return(temp + rest) }
     ; addup indexes }
     

main =
  do { arr <- newListArr (1,9) [7,4,5,9,8,1,2,3,6]
     ; printArr arr
     ; revArray arr
     ; printArr arr
     ; sum <- sumArr arr
     ; print sum
     }













{-     
printEach :: Show a => Array a -> IO ()
printEach a =
  do { (low,high) <- boundsArr a
     ; worker (low,high) a
     ; return ()
     }
     
worker (low,high) a | low > high = return ()
worker (low,high) a =
  do { item <- readArr a low
     ; print item
     ; worker (low+1,high) a
     }

revIndexes :: (Int,Int) -> [(Int,Int)]
revIndexes (low,high) = zip countup countdown
  where n = high - low + 1
        half = n `div` 2
        countup = [low..half]
        countdown = [high, high -1 .. low]
     
rev5 :: Array a -> IO ()
rev5 a = 
  do { (low,high) <- boundsArr a
     ; let scan [] = return ()
           scan ((i,j):xs) = do { swap a (i,j); scan xs }
     ; scan (revIndexes (low,high))
     }
-}     
   
     