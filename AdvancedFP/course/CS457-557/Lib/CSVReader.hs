module CSVReader  where

-- Based on notes http://web.cecs.pdx.edu/~sheard/course/CyberMil/Notes/monads.pptx
import Control.Monad.State
import CSV

--------------------------------------------
-- Try 1

getString:: [String] -> (String,[String])
getString (s:ss) = (s,ss)
getString [] = error "No more strings to read a 'String' from"

getInt:: [String] -> (Int,[String])
getInt (s:ss) = (read s,ss)
getInt [] = error "No more strings to read an 'Int' from"

getInts:: Int -> [String] -> ([Int],[String])
getInts 0 ss = ([],ss)
getInts n ss = case getInt ss of
                 (x,ss2) -> case getInts (n-1) ss2 of
                              (xs,ss3) -> (x:xs,ss3)
                              
getLine6:: [String] -> ((String,[Int]),[String])
getLine6 ss = case getString ss of
                (count,ss2) -> case getInts 3 ss2 of
                                 (rolls,ss3) -> ((count,rolls),ss3)

--------------------------------------------
-- Try 2.
-- Use a monad
-- Define a function for each type
-- Separate reading a line from reading a file.

-- data State s a = State (s -> (a,s))
type Line a = State [String] a


bool:: Line Bool
bool = (state f)
  where f ("True" : ss) = (True,ss)
        f ("False" : ss) = (False,ss)
        f (x:xs) = error ("Non Bool in reader bool: "++x)
        f [] = error "No more strings to read a 'Bool' from"

string:: Line String
string = state f
  where f (s:ss) = (s,ss)
        f [] = error "No more strings to read a 'String' from"
        
int:: Line Int
int = mapState f (return ())
  where f ((),s:ss) = (read s,ss)
        f ((),[]) = error "No more strings to read an 'Int' from"
        
skip:: Line ()
skip = state f
  where f (s:ss) = ( (), ss)
        f [] = error "No more strings to 'skip' over"

list:: Int -> Line a -> Line [a]
list 0 r = return []
list n r = do { x <- r; xs <- list (n-1) r; return(x:xs)}

blank:: Line a -> Line(Maybe a)
blank m = state f 
  where f ("":xs) = (Nothing, xs)
        f  xs = case (runState m) xs of
                  (y,ys) -> (Just y,ys)

--------------------------------------------
-- Use these to build readers for a whole line

getLine1 :: Line [String]
getLine1 =
   do { skip
      ; list 3 string
      }
      
getLine2_18 :: Line (String,[Maybe Int])
getLine2_18 = 
   do { count <- string
      ; rolls <- list 3 (blank int)
      ; return(count,rolls)
      }

-------------------------------------------
-- Reading a whole file of Comma Separated Values
-- which is represented by [[String]]

type Reader a = State [[String]] a

lineToReader:: Line a -> Reader a
lineToReader l1 = mapState f (return ())
  where f ((),line:lines) = (evalState l1 line,lines)

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
        f (Right xs) = return(evalState reader xs)

--------------------------------------------------
-- Put it all together for a first test

get3DieExample :: Reader ([String],[(String,[Maybe Int])])     
get3DieExample =
  do { labels <- lineToReader getLine1
     ; pairs <- getN 18 getLine2_18
     ; return(labels,pairs)
     }
     
test1 = importCSV get3DieExample "roll3Die.csv" 

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

get3DieEx2 = 
       (row snd (skip `x` list 3 string)) 
  `x`  (many 18 (row id  (string `x`  list 4 (blank int))))

test2 = importCSV get3DieEx2 "roll3Die.csv" 

