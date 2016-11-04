module CSVReader2 where

import Control.Monad.State
import CSV

----------------------------------------------
-- Keep track of lines and columns in the state
-- part of the monad
-- Define a function for each type
-- Separate reading a line from reading a file.

-- data State s a = State (s -> (a,s))
type Line a = State (Int,Int,[String]) a

report l c message =
  error ("\n at line: "++show l++", column: "++show c++"\n   "++message)

bool:: Line Bool
bool = (state f)
  where f (l,c,"True" : ss) = (True,(l,c+1,ss))
        f (l,c,"False" : ss) = (False,(l,c+1,ss))
        f (l,c,x:xs) = report l c ("Non Bool in reader bool: "++x)
        f (l,c,[]) = report l c "No more strings to read a 'Bool' from"

string:: Line String
string = state f
  where f (l,c,s:ss) = (s,(l,c+1,ss))
        f (l,c,[]) = report l c "No more strings to read a 'String' from"
        
int:: Line Int
int = mapState f (return ())
  where f ((),(l,c,s:ss)) = (read s,(l,c+1,ss))
        f ((),(l,c,[])) = report l c "No more strings to read an 'Int' from"
        
skip:: Line ()
skip = state f
  where f (l,c,s:ss) = ( (), (l,c+1,ss))
        f (l,c,[]) = report l c "No more strings to 'skip' over"

list:: Int -> Line a -> Line [a]
list 0 r = return []
list n r = do { x <- r; xs <- list (n-1) r; return(x:xs)}

blank:: Line a -> Line(Maybe a)
blank m = state f 
  where f (l,c,"":xs) = (Nothing, (l,c+1,xs))
        f  xs = case runState m xs of
                  (y,ys) -> (Just y,ys)


-------------------------------------------
-- Reading a whole file of Comma Separated Values
-- which is represented by [[String]]

type Reader a = State (Int,[[String]]) a


lineToReader:: Line a -> Reader a
lineToReader l1 = mapState f (return ())
  where f ((),(l,line:lines)) = (evalState l1 (l,1,line),(l+1,lines))

getN ::  Int -> Line a -> Reader [a]
getN 0 line = return []
getN n line =
  do { x <- lineToReader line
     ; xs <- getN (n-1) line
     ; return(x:xs)
     }

importCSV :: Reader a -> String -> IO a
importCSV reader file = 
      do { r <- parseCSVFromFile file; (f r)}
  where f (Left err) = error ("Error reading from file: "++
                              file++"\n"++show err)
        f (Right xs) = return(evalState reader (1,xs))

--------------------------------------------
-- Generic Monad operations

infixr 3 `x`
x :: Monad m => m b -> m c -> m(b,c)     
r1 `x` r2 = do { a <- r1; b <- r2; return(a,b) } 

many :: Monad m => Int -> m c -> m [c]
many n r = sequence (replicate n r)
  where -- sequence is in the Prelude but we provide
        -- a definition here because it is interesting
        sequence [] = return []
        sequence (c:cs) = do { x <- c
                             ; xs <- sequence cs
                             ; return(x:xs)}   

row:: (a -> b) -> Line a -> Reader b
row f line1 = lineToReader line2
  where line2 = do { x <- line1; return(f x) }

get3DieEx2 :: Reader ([[Char]],[([Char],[Maybe Int])])
get3DieEx2 =      (row snd (skip `x` list 3 string)) 
             `x`  (many 18 (row id cols2_18))
 where cols2_18 = (string `x` list 4 (blank int))


test2 = importCSV get3DieEx2 "roll3Die.csv" 

