module ArrayExercise where

import ArrCommands

{- ArrCommands defines the following operations on Arrays

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

main =
  do { arr <- newArr (1,6) 99
     ; x <- printArr arr
     ; writeArr  arr 4 12
     ; printArr arr
     ;  counts <- newListArr (0,5) [1..5]
    -- ; writeArr counts 5 33
     --; printArr counts
     ; printSomeArr 5 counts
     }
     