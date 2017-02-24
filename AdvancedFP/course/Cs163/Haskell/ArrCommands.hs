module ArrCommands(Array,boundsArr
                    ,writeArr,readArr
                    ,newArr, newListArr
                    ,toListArr
                    ,showArr, printArr, printSomeArr
                    ) where


import Data.Array.IO

type Array a = IOArray Int a

boundsArr:: Array a -> IO(Int,Int)
boundsArr x = getBounds x

writeArr:: Array a -> Int -> a -> IO ()
writeArr array index elem = writeArray array index elem

readArr:: Array a -> Int -> IO a
readArr array index = readArray array index  

newArr :: (Int,Int) -> a -> IO(Array a)
newArr bounds init = newArray bounds init

newListArr:: (Int,Int) -> [a] -> IO(Array a)
newListArr = newListArray

toListArr:: Array a -> IO [a]
toListArr = getElems

showArr:: Show a => Array a -> IO String
showArr x = 
   do { b <- boundsArr x
      ; xs <- toListArr x
      ; return("newListArray"++show b++show xs)}

printArr:: Show a => Array a -> IO ()
printArr x = do { s <- showArr x; putStrLn s }

printSomeArr n x =
  do { xs <- toListArr x
     ; b <- boundsArr x    
     ; let ys = map show (take n xs) ++ map (\ _ -> "_") (drop n xs)
           plist [] = ""
           plist [x] = x
           plist (x:xs) = x++","++plist xs
     ; putStrLn("newListArray "++show b++"["++plist ys++"]") }

