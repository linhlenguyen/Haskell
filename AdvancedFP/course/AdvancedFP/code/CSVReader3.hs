module CSVReader3  where

import Control.Monad.State
import CSV

--------------------------------------------------
-- Line and Table Readers with position tracking

type Line  a = State (Int,Int,[String]) a
type Table a = State (Int,[[String]]) a

report l c message =
  error ("\n at line: "++show l++", column: "++show c++"\n   "++message)
 
tab:: Line a -> Table a
tab m = (state g)  
 where g (l,line:table) = let (x,(lin,col,extra)) = runState m (l,0,line)
                          in (x,(l+1,table))

------------------------------------------------------------
-- How to read particular types

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

blank:: Line a -> Line(Maybe a)
blank m = state f 
  where f (l,c,"":xs) = (Nothing, (l,c+1,xs))
        f  xs = case runState m xs of
                  (y,ys) -> (Just y,ys)

------------------------------------------------------------
-- generic operators for any monad

infixl 3 `x`
infixl 3 `x2`
infixl 3 `x3`
infixl 3 `x4`
infixl 3 `bind`

bind x f = do { a <- x; b <- f a; return(a,b)}


r1   `x`  x = do { a <- r1; b <- x; return(a,b) } 
pair `x2` x = do { (a,b) <- pair; c <- x; return(a,b,c)}
trip `x3` x = do { (a,b,c) <- trip; d <- x; return(a,b,c,d)}
quad `x4` x = do { (a,b,c,d) <- quad; e <- x; return(a,b,c,d,e)}

list:: Monad m => m a -> Int -> m [a]
list r 0 = return []
list r n = do { x <- r; xs <- list r (n-1); return(x:xs)}

-------------------------------------------------
-- an example reader

ww =   tab  (skip >> list string 3)                `x`
       list (tab (int `x` list (blank int) 3)) 18  `x2`
       tab  (int `bind` list bool)
       

 