module Graph where

import ArrCommands

--------------------------------------
-- Simple generic graphs

type Graph vertex = vertex -> [vertex]

graph1::  Graph Int
graph1 6 = [4]
graph1 5 = [1,2,4]
graph1 4 = [3,5,6]
graph1 3 = [4,2]
graph1 2 = [1,3,5]
graph1 1 = [2,5]
graph1 _ = []

-------------------------------
-- Directed Graphs

type Dgraph vertex = vertex -> [vertex]

data Node = A | B | C | D | E
  deriving (Show,Eq)

graph2:: Dgraph Node
graph2 A = [B]
graph2 B = [C,D,E]
graph2 C = []
graph2 D = [B,C]
graph2 E = [D]
-- graph2 _ = []

------------------------------------------
-- Vertex labeled graphs

type VLgraph label vertex = vertex -> ([vertex],label)

data Color = Blue | Red | Yellow | Green
  deriving (Show,Eq)

graph4:: VLgraph Color Int
graph4 2 = ([4],Blue)
graph4 3 = ([7],Yellow)
graph4 4 = ([3,5],Blue)
graph4 5 = ([7],Red)
graph4 6 = ([2],Red)
graph4 7 = ([6],Green)
graph4 _ = ([],undefined)


------------------------------------------------
-- Edge labelled graphs

type ELgraph label vertex = vertex -> [(vertex,label)]

graph6:: ELgraph Int Int
graph6 1 = [(4,5),(2,2)]
graph6 2 = [(1,2),(4,5),(3,14),(5,4)]
graph6 3 = [(2,14),(5,34)]
graph6 4 = [(1,5),(2,5),(5,58)]
graph6 5 = [(2,4),(3,34),(4,58)]
graph6 _ = []


graph7:: Graph Int
graph7 1 = [7]
graph7 2 = [6]
graph7 3 = [1,5]
graph7 4 = [6]
graph7 5 = [2,4]
graph7 6 = [8]
graph7 7 = [2,8]
graph7 8 = []
graph7 _ = []

-----------------------------------------------
type ArrGraph i = Array [i]

vertices:: ArrGraph i -> IO[Int]
edges:: ArrGraph i -> IO[(Int,i)]
children:: ArrGraph i -> Int -> IO[i]


vertices g = 
  do { (lo,hi) <- boundsArr g
     ; return [lo..hi]}
     
edges g = 
  do { (low,hi) <- boundsArr g
     ; ees <- toListArr g
     ; return [ (i,j) | (i,cs) <- zip [low..hi] ees, j <- cs ]
     }
 
 
children g node = readArr g node


-----------------------------------------------------------
-- Slight variations on graphs in the array representation

type VLArrGraph label = Array ([Int],label)
type ELArrGraph label = Array [(Int,label)]

---------------------------------------------------------
-- Algorithms on Graphs

-- Searching a graph, find a graph node whose vertex label
-- meets some property p.

-- First using graphs represented as pure functions

search:: Eq v => VLgraph sat v -> (sat -> Bool) -> [v] -> Maybe (v,sat)
search graph p vs = help vs []
 where help [] visited = Nothing
       help (r:rs) visited | elem r visited = help rs visited
       help (r:rs) visited = if p label
                                then Just(r,label)
                                else help (rs++children) (r:visited)
         where (children,label) = graph r

test1 = search graph4 (==Green) [2]

-- Second using vertex labelled graphs represented as Arrays

search2:: Array ([Int], t) -> 
         (t -> Bool) -> 
         [Int] -> 
         IO (Maybe (Int, t))
         
search2 graph p vs = help vs []
 where help [] visited = return Nothing
       help (r:rs) visited | elem r visited = help rs visited
       help (r:rs) visited = 
         do { (children,label) <- readArr graph r
            ; if p label
                 then return(Just(r,label))
                 else help (rs++children) (r:visited) }

--------------------------------------------------------
-- Reachability, Can you get from A to B, is best asked
-- by computing all nodes reachable from A, and then testing
-- if B is in that set.

-- reaches:: Eq v => Graph v -> v -> [v]
reaches graph v = help [v] []
  where help [] visited = visited
        help (p:pending) visited | elem p visited = help pending visited
        help (p:pending) visited = help (pending++children) (p:visited)
          where (children,_) = graph p


-- computing paths, is similar to reachability, but we keep track
-- of paths rather than nodes.

pathsFrom graph v = help [(v,[])]
  where help [] = []
        help ((p,ps):pending) = (p:ps) : (help (add children pending))
          where children = graph p
                add [] qs = qs
                add (c:cs) qs = 
                   if elem c ps      -- adding c would cause a cycle
                      then add cs qs -- so skip over that path
                      else add cs ((c,p:ps):qs)
                 
test2 = pathsFrom graph2 B 

allPaths :: (Eq t) => (t -> [t]) -> t -> t -> [[t]]
allPaths graph start finish = goodpaths
  where allpaths = pathsFrom graph start
        goodpaths = filter (\ (x:xs) -> x==finish) allpaths
        
test3 = allPaths graph2 A C     

--------------------------------------------------
-- Cycle detection on directed graphs


         