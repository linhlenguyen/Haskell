-------------------

import IOActions

-------------------
-- 1) How many Haskell source files are there in the
--    current directory?

endInHs str = case reverse str of
                ('s':'h':'.':_) -> True
                ('s':'h':'l':'.':_) -> True
                _ -> False
                

haskelSourceFiles = 
     getCurrentDirectory
     >>= getDirectoryContents
     >>= inIO (filter endInHs)
     >>= inIO length

a1 = do current <- getCurrentDirectory
        files <- getDirectoryContents current
        return( length (filter endInHs files) )

-------------------
-- 2) How many lines of Haskell source code are in
--    the current directory?

countLines:: String -> IO Int
countLines s = 
       readFile s
   >>= inIO (length . lines)

countLines2 s = 
   do contents <- readFile s
      return(length(lines contents))

linesOfCode = 
         getCurrentDirectory
     >>= getDirectoryContents
     >>= inIO (filter endInHs)
     >>= mapM countLines
     >>= inIO sum
     
a2 = do current <- getCurrentDirectory
        files <- getDirectoryContents current
        let hsfiles = (filter endInHs files)
        counts <- mapM countLines2 hsfiles
        return(sum counts)

-------------------
-- 3) What is the largest Haskell source file in the
--    current directory

countLines' s = 
       readFile s
   >>= inIO (length . lines)
   >>= inIO (\ n -> (n,s))

countLines'2 s =
  do contents <- readFile s
     return(length(lines contents) ,s)

--   >>= (\ n -> return(n,s))

largestFile = 
         getCurrentDirectory
     >>= getDirectoryContents
     >>= inIO (filter endInHs)
     >>= mapM countLines'
     >>= inIO sort
     >>= inIO (snd . head . reverse)

a3 = do current <- getCurrentDirectory
        files <- getDirectoryContents current
        let hsfiles = (filter endInHs files)
        counts <- mapM countLines'2 hsfiles
        print counts
        return(snd(head(reverse (sort counts))))
        
-------------------
-- 4) Copy the largest Haskell source file in the
--    current directory into Largest.hs

copyLargest = 
         largestFile
    >>=  readFile
    >>= writeFile "Largest.hs"

a4 = copyLargest

-------------------
