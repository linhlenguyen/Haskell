module GameTree where

import ArrCommands

----------------------------------------------------------------
-- A game tree has a game state (board configuration) as a type
-- parameter, and a list of GameTrees as possible moves

data GameTree a = Node a [GameTree a]

------------------------------------------------------
-- Because GameTrees have the peculiar structure of
--    1) Having only one constructor
--    2) Having a list of GameTrees as a component
-- functions over game trees often come in pairs.
--  a) One over (GameTree a)
--  b) Another over [GameTree a]
--  c) The functions are mutually recursive (they call one another)
-- For example to add up all integers in a (GameTree Int) we might 
-- write two functions "sumGameTree" and "sumTrees"

sumGameTree :: GameTree Int -> Int
sumGameTree (Node a ts) = a + sumTrees ts

sumTrees :: [GameTree Int] -> Int
sumTrees [] = 0
sumTrees (t:ts) = sumGameTree t + sumTrees ts


----------------------------------------------------------
-- Searching a Gametree for the first configuration
-- that meets some property is quite common. For example
-- if the game was tic-tac-toe, one might search for a
-- move that wins in one step, or a move that prevents an 
-- imminent winning move by an opponent. 
-- Like other operations, searching is done with a pair 
-- of functions

searchGameTree:: (a -> Bool) -> GameTree a -> Maybe(GameTree a)
searchGameTree pred (Node a ts) 
   | pred a = Just(Node a ts)
   | True   = searchTrees pred ts
   
searchTrees:: (a -> Bool) -> [GameTree a] -> Maybe(GameTree a)   
searchTrees pred [] = Nothing
searchTrees pred (t:ts) = 
  case searchGameTree pred t of
    Just t -> Just t
    Nothing -> searchTrees pred ts

---------------------------------------------------------    
-- The algorithm above performs a depth-first search
-- of the game tree. To perform a breadth-first search
-- we need an additional parameter of "pending" trees 
-- to search.

bfs:: (a -> Bool) -> GameTree a -> [GameTree a] -> Maybe(GameTree a)
bfs pred (Node a []) [] | not(pred a) = Nothing
bfs pred (Node a ts) pending 
  | pred a = Just(Node a ts)
  | True   = case pending of
              [] -> bfs pred (Node a []) ts
              (p:ps) -> bfs pred p (ps ++ ts)
              
-- Note that this approach combines the two functions into one.
-- Could we use this strategy to rewrite the sumGameTree function?

sumTree:: GameTree Int -> [GameTree Int] -> Int
sumTree (Node a [])     []      = a 
sumTree (Node a (t:ts)) pending = a + sumTree t (ts ++ pending)
sumTree (Node a [])     (p:ps)  = a + sumTree p ps

---------------------------------------------------------------
-- This strategy can be used for depth first search as well.
-- we just have to be careful how we extend the pending list.

dfs :: (a -> Bool) -> GameTree a -> [GameTree a] -> Maybe(GameTree a)
dfs pred (Node a []) [] | not(pred a) = Nothing
dfs pred (Node a ts) pending 
  | pred a = Just(Node a ts)
  | True   = case pending of
              [] -> dfs pred (Node a []) ts
              (p:ps) -> dfs pred p (ts ++ ps)

-------------------------------------------------------------------
-- In fact, we can abstract over how we deal with the pending list

search :: (a -> Bool) -> ([GameTree a] -> [GameTree a] -> [GameTree a]) ->
          GameTree a -> [GameTree a] -> Maybe(GameTree a)
search pred add (Node a ts) pending 
  | pred a = Just(Node a ts)
  | True   = case pending of
              [] -> search pred add (Node a []) ts
              (p:ps) -> search pred add p (add ts ps)
              
dfs' pred t = search pred (++) t []

bfs' pred t = search pred (\ ts ps -> ps ++ ts) t []

----------------------------------------------------------------------------
-- Note that dfs' use a stack-like approach because newly-added
-- trees are processed first, and bfs' uses a queue-like approach
-- as trees are added to the back of the queue.
-- Now, think about the cost. In the worst case we visit every-node 
-- in the tree. So the algorithm is at least linear. Are 
-- there other costs? It costs to build the pending list with add.
-- What is more expensive stack-like or queue-like beavior?
-- Can we do better. Well, maybe if we don't always use lists
-- to represent the pending list.

{-
data Container a

add:: [a] -> Container a -> Container a
remove:: Container a -> (a,Container a)
isEmpty:: Container a -> Bool

add items container = undefined
remove container = undefined
isEmpty container = undefined

search2 pred (Node a []) pending | not(p a) && isEmpty pending = Nothing
search2 pred (Node a ts) pending 
  | pred a = Just(Node a ts)
  | True   = if isEmpty pending
                then search2 pred (Node a []) (add ts pending)
                else case remove pending of
                      (p,ps) -> search2 pred p (add ts ps)
-}

---------------------------------------------------------------
-- This scheme, where we have
--    1) Some data-structure (Container a)
--    2) And some operators over that structure (add, remove, isEmpty)
-- is very common, and Haskell has a mechanism to implement this
-- called the "class system". We have been using it all the time, we
-- have called these things constrained types, or type predicates.
-- Here are some well known examples 
--     1) the type is numeric types: t
--     2) the operators are (+) (-) (*) etc
--     3) the class is called "(Num t)"

--     1) the type is orderable types: t
--     2) the operators are (<) (<=) (compare) etc
--     3) the class is called "(Ord t)"

--     1) the type is types with equality: t
--     2) the operators are (==) (/=) etc
--     3) the class is called "(Eq t)"

class Container c where
  add:: [a] -> c a -> c a
  remove:: c a -> (a,c a)
  isEmpty:: c a -> Bool
  empty:: c a


search2:: (Container c) => 
          (t -> Bool) -> GameTree t -> c (GameTree t) -> Maybe (GameTree t)  

search2 pred (Node a []) pending | not(pred a) && isEmpty pending = Nothing          
search2 pred (Node a ts) pending 
  | pred a = Just(Node a ts)
  | True   = if isEmpty pending
                then search2 pred (Node a []) (add ts pending)
                else case remove pending of
                      (p,ps) -> search2 pred p (add ts ps)  
                      
-- Now lets make simple "instances", one for stacks, and one for queues.

data Stack a = Stack [a]
data QueueE a = QueueE [a]

instance Container Stack where
  add xs (Stack ys) = Stack(xs++ys)
  remove (Stack (x:xs)) = (x,Stack xs)
  isEmpty (Stack xs) = null xs
  empty = Stack []
  
instance Container QueueE where
  add xs (QueueE ys) = QueueE(ys++xs)
  remove (QueueE (x:xs)) = (x,QueueE xs)
  isEmpty (QueueE xs) = null xs
  empty = QueueE []  

--------------------------------------------------------------
-- Now its easy to make both depth and breadth first searches

dfs2 pred t = search2 pred t (Stack [])

bfs2 pred t = search2 pred t (QueueE [])


-------------------------------------------------------------
-- What's the expected cost for the 4 operations 
-- operation         Stack           QueueE
-- 
--   add             constant        proportional to n, where n is size of Queue
--   remove          constant        constant
--   isEmpty         constant        constant
--   empty           constant        constant


-------------------------------------------------------
-- Can we make an efficient Queue container with constant
-- add costs? Yes!!

data Queue a = Queue [a] [a]

-- Use a pair of lists.The first list is the front of the queue
-- the second list is the reverse of the rear of the queue. For eaxmple
-- A queue with ordered elements    [1,6,3,4,7,23,98,3] 
--                                  front          rear
-- Could be represented as  (Queue  [1,6,3,4,7]    [3,98,23])
-- removing an element      (1, Queue  [6,3,4,7]   [3,98,23])
-- adding a element 33      (Queue  [1,6,3,4,7]    [33,3,98,23])
-- Both are constant time operations.
-- What if the front part is empty when we want to remove an element
-- remove (Queue [] [3,4,5]) ----->  (5,Queue [4,3] [])

instance Container Queue where
  add xs (Queue front rear) = Queue front (xs++rear)
  remove (Queue (x:xs) rear) = (x,Queue xs rear)
  remove (Queue [] rear) = (x,Queue front []) where (x:front) = reverse rear
  isEmpty (Queue front rear) = null front && null rear
  empty = Queue [] []  

showQ2 (Queue front revrear) = putStrLn str
   where fs = show front
         fr = show revrear
         fpos = 4
         rpos = length fr + 8
         str = tag fpos "front" ++ "\n" ++
               tag fpos "|" ++ "\n" ++ 
               "   "++fs++"    "++fr ++ "\n"++
               tag rpos "|" ++ "\n" ++
               tag rpos "reverse of the rear" ++ "\n"

       
-----------------------------------------------------------
-- Can we make efficient Stacks and Queues with arrays?

data ArrStack a = ArrStack Int Int (Array a)

newStack n a = 
  do { arr <- newArr (0,n) a
     ; return(ArrStack 0 n arr)}

     
addStack :: a -> ArrStack a -> IO (ArrStack a)
addStack a (ArrStack top hi arr) 
   | top <= hi = do { writeArr arr top a
                   ; return(ArrStack (top+1) hi arr)}
   | True = error "Stack full"
   
   
delStack :: ArrStack a -> IO (a,ArrStack a)
delStack (ArrStack top hi arr) 
   | top > 0 = do { let newtop = top-1
                  ; ans <- readArr arr newtop
                  ; return(ans,ArrStack newtop hi arr)}
   | True = error "Stack empty"   


isEmptyStack (ArrStack top hi arr) = top==0

----------------------------------------------
-- Displaying stacks for a demo

showStackSlot top n x
  | n<top = show x
  | True = "_"
    
showSt (ArrStack top hi arr) =
  do { elems <- toListArr arr
     ; let xs = zipWith (showStackSlot top) [0..] elems
           tpos = pos top xs
           str = tag tpos ("top = "++show top++ "\n") ++
                 tag tpos "|\n["     ++
                 plistf id xs  ++ "]" 
     ; putStrLn ("\n"++str)
     }

-- loop inside demo

loop:: String -> ArrStack Int -> IO ()
loop ('a':more) stack = 
    do { st <- addStack (read more) stack
       ; showSt st
       ; next <- getLine
       ; loop next st
       }
loop ('d':more) stack = 
    do { (ans,st) <- delStack stack
       ; putStrLn ("\n\nelement removed = "++show ans)
       ; showSt st
       ; next <- getLine
       ; loop next st
       }       
loop _ _ = return ()

demoSt = 
  do { st <- newStack 6 (-99)
     ; showSt st
     ; next <- getLine
     ; loop next st
     }


demoQ2 = 
    do { let (q::Queue Int) = empty
       ; showQ2 q
       ; next <- getLine
       ; loop2 next q
     }
     
loop2:: String -> Queue Int -> IO ()
loop2 ('e':more) q0= 
    do { let q1 = add [read more] q0
       ; showQ2 q1
       ; next <- getLine
       ; loop2 next q1
       }
loop2 ('d':more) q0 = 
    do { let (ans,q1) = remove q0
       ; putStrLn ("\n\nelement removed = "++show ans)
       ; showQ2 q1
       ; next <- getLine
       ; loop2 next q1
       }       
loop2 _ _ = return ()     
--------------------------------------------------
-- generic stuff for displaying

pos 0 xs = 1
pos n (x:xs) = length x +1 + pos (n-1) xs

tag n s = replicate n ' '++s

plistf f [x] = f x
plistf f [] = ""
plistf f (x:xs) = f x ++ "," ++ plistf f xs
  
   
   
