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
revArray arr = undefined

swap:: Array a -> (Int,Int) -> IO ()
swap x is = undefined

indices :: Int -> [(Int,Int)]
indices x = undefined

iterate:: [(Int,Int)] -> IO ()
iterate x = undefined

main =
  do { arr <- newListArr (1,9) [7,4,5,9,8,1,2,3,6]
     ; printArr arr
     ; revArray arr
     ; printArr arr
     }
     
     