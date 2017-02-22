-------------------

import IOActions

{- Study the solutions to the last in class exercise.
Re-do each example, except, instead of using the "pipe-line"
operators (>>=) (inIO) etc. use the d0 notation.
I suggest you comment out, but leave in place the old solution
for reference as you work.
-}

-------------------
-- 1) How many Haskell source files are there in the
--    current directory?

haskelSourceFiles = 
      getCurrentDirectory
  >>= getDirectoryContents
  >>= inIO (filter isHaskell)
  >>= inIO length
  >>= print
  

isHaskell s = case (reverse s) of
                ('s':'h':'.':xs) -> True
                other -> False
  
  
a1 = haskelSourceFiles

-------------------
-- 2) How many lines of Haskell source code are in
--    the current directory?

linesOfCode = undefined 


a2 = linesOfCode

-------------------
-- 3) What is the largest Haskell source file in the
--    current directory

largestFile = undefined 

a3 = largestFile
-------------------
-- 4) Copy the largest Haskell source file in the
--    current directory into Largest.hs

copyLargest = undefined

a4 = copyLargest

-------------------
