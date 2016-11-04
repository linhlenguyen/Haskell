module IOActionExamples where

import IOActions
import Treedot(toDot)
import Graphviz


x1 = return "./"
     >>= getFileSystem 4
     >>= inIO toDot
     >>= writeFile "fileSys.dot"
     

draw f1 f2 = system ("dot -Tpng "++f1++" > "++f2)  

view filename = system ("explorer "++filename) 

x2 = return "D:/work/sheard/scripts"
     >>= getFileSystem 4
     >>= inIO toDot
     >>= writeFile "fileSys.dot"
     >> draw "fileSys.dot" "fileSys.png"
     >> view "fileSys.png"
     
     
endsIn ('s' : 'h' : '.' : _) = True
endsIn ('s' : 'h' : 'l' : '.' : _) = True
endsIn x = False

isHaskell x = endsIn (reverse x)

x3 =  getDirectoryContents "."
      >>= inIO (length . filter isHaskell)
      >>= print
      
nlines s = length(lines s)
nfile s = readFile s >>= inIO nlines


x4 =  getDirectoryContents "."
      >>= inIO (filter isHaskell)
      >>= mapM nfile
      >>= inIO sum
      >>= print
      
nfileName s = readFile s >>= inIO (\ contents -> (nlines contents,s))
      
x5 =  getDirectoryContents "."
      >>= inIO (filter isHaskell)
      >>= mapM nfileName  
      >>= inIO (snd . maximum)
      
x6 = x5
     >>= readFile
     >>= writeFile "Largest.hs"
     
echo = do c <- getChar
          putChar c
          echo     
     
     
