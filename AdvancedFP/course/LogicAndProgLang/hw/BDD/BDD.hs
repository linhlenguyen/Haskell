{-# LANGUAGE  FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}

module BDD where

import qualified Control.Monad.State as MS
import qualified Data.Map as DM
import qualified FiniteSet as FS
import Data.List(sortBy)

import qualified Data.Array as AR
import Graphviz hiding (Shape)


---------------------------------------------------------
-- BDD Trees

data Tree = Leaf Bool | Node String Int Int deriving Show

instance  Ord Tree where
  compare (Leaf x) (Leaf y) = compare x y
  compare (Leaf _) _ = LT
  compare _ (Leaf _) = GT
  compare (Node s y x) (Node t m n) = 
    case compare s t of
      EQ -> case compare y m of
              EQ -> compare x n
              y -> y
      y -> y   
      
instance Eq Tree where
  x == y = case compare x y of {EQ -> True; _ -> False}

-------------------------------------------------------
-- BDDs are an index and a pair of tables

type Triple =  (Int,DM.Map Tree Int,DM.Map Int Tree)
type BDD = MS.State Triple Int

---------------------------------------------
-- monadic operations
   
next2 :: Num i => MS.State (i,t1,t2) i
next2 = MS.mapState f (return ())
  where f ((),(i,m1,m2)) = (i,(i+1,m1,m2))

find2 :: Tree -> MS.State Triple (Maybe Int)
find2 x = MS.mapState f (return ())
  where f ((),(i,tab1,tab2)) = (DM.lookup x tab1,(i,tab1,tab2))

update2 :: Int -> Tree -> MS.State Triple Int
update2 n term = MS.mapState f (return ())
  where f ((),(i,tab1,tab2)) = (n,(i,DM.insert term n tab1,DM.insert n term tab2))

node2:: Int -> MS.State Triple Tree
node2 x = MS.mapState f (return ())
  where f ((),(i,tab1,tab2)) = 
              case DM.lookup x tab2 of
                Just ans -> (ans,(i,tab1,tab2))
                Nothing -> error ("Missing node: "++show x)

indexes:: MS.State Triple [Int]
indexes = MS.mapState f (return ())
  where f ((),(i,tab1,tab2)) = (DM.keys tab2,(i,tab1,tab2))

----------------------------------------
-- operations on BDDs 

runBDD:: BDD -> (Int,Triple)
runBDD x = MS.runState x stateZero

showBDD :: BDD -> String
showBDD x = unlines (map f (DM.toList tab2))
  where (i,(next,tab1,tab2)) = runBDD x
        f (k,a) | k==i = (" -> "++show k++" "++show a)
        f (k,a)        = ("    "++show k++" "++show a)

instance Show BDD where
  show x = showBDD x

lift:: (Bool -> Bool -> Bool) -> Bool -> Bool -> BDD
lift oper x y = 
  do { b <- find2(Leaf (oper x y))
     ; case b of 
         Just n -> return n
         Nothing -> error(if (oper x y) then noTruth else noAbsurd) }
         
-------------------------------------------------
-- Since a BDD is a monadic stateful computation
-- we need an initial state.

fIndex = 1
tIndex = 2

-- every BDD should have just true and false in it to start

stateZero :: Triple
stateZero = (3,tab1,tab2)
  where tab1 = DM.insert (Leaf False) fIndex (DM.insert (Leaf True) tIndex DM.empty)
        tab2 = DM.insert fIndex (Leaf False) (DM.insert tIndex (Leaf True) DM.empty)

noTruth  = "true not found in table. Initialization error?"
noAbsurd = "false not found in table. Initialization error?"


------------------------------------------------------

instance FS.Boolean BDD where
  true = BDD.true
  false = BDD.false
  isTrue = BDD.isTrue
  isFalse = BDD.isFalse
  conj = BDD.conj
  disj = BDD.disj
  neg = BDD.neg
  imply = BDD.imply
  

------------------------------------------------------
-- operations for building BDDs

true :: BDD
true = 
 do { b <- find2 (Leaf True)
    ; case b of {Just n -> return n; Nothing -> error noTruth }}
    
false :: BDD
false = 
 do { b <- find2 (Leaf False)
    ; case b of {Just n -> return n; Nothing -> error noAbsurd }}    

conj x y = apply2 (&&) x y

disj x y = apply2 (||) x y

imply x y = apply2 imp x y
  where imp False x = True
        imp True x = x
        
neg x = apply2 (||) x true        
        
isTrue:: BDD -> Bool  
isTrue x = case DM.lookup i tab2 of
             Just(Leaf True) -> True
             _ -> False
  where (i,(next,tab1,tab2)) = runBDD x

isFalse:: BDD -> Bool  
isFalse x = case DM.lookup i tab2 of
             Just(Leaf False) -> True
             _ -> False
  where (i,(next,tab1,tab2)) = runBDD x  

letter s = node s false true

x = letter "x"
y = letter "y"
z = letter "z"

---------------------------------------------------------- 
-- memoizing building of Tree nodes with all common 
-- subexpressions collapsed into a single node

node:: String -> BDD -> BDD -> BDD
node s x y = 
  do { a <- x
     ; b <- y
     ; makeNode s a b }

makeNode:: String -> Int -> Int -> BDD     
makeNode s a b = 
    if a==b 
       then return a                 -- CASE 1 page 373
       else findOrCreate (Node s a b)

findOrCreate:: Tree -> BDD        
findOrCreate term =  
  do { test <- find2 term
     ; case test of
         Just n -> return n          -- CASE 2 page 373
         Nothing -> do { n <- next2  -- CASE 3 page 373
                       ; update2 n term
                       ; return n }}
                               
applyI:: (Bool -> Bool -> Bool) -> Int -> Int -> BDD                      
applyI oper n m =
  do { x1 <- node2 n
     ; y1 <- node2 m
     ; case (x1,y1) of 
         (Leaf n,Leaf m) -> lift oper n m           -- Case 1 page 375
         (Node x lx hx,Node y ly hy) | x==y ->      -- Case 2 page 375
            do { lo <- applyI oper lx ly
               ; hi <- applyI oper hx hy
               ; (makeNode x lo hi) } 
         (Node x lx hx,Node y ly hy) | y>x ->       -- Case 3a page 375
            do { lo <- applyI oper lx m
               ; hi <- applyI oper hx m
               ; (makeNode x lo hi) } 
         (Node x lx hx,Node y ly hy) | y<x ->       -- Case 4a page 375
            do { lo <- applyI oper n ly
               ; hi <- applyI oper n hy
               ; (makeNode y lo hi) }               
         (Node x lx hx,Leaf _) ->                   -- Case 3b page 375
            do { lo <- applyI oper lx m
               ; hi <- applyI oper hx m
               ; (makeNode x lo hi) }
         (Leaf _,Node y ly hy) ->                   -- Case 4b page 375
            do { lo <- applyI oper n ly
               ; hi <- applyI oper n hy
               ; (makeNode y lo hi) }         
     }

     
apply:: (Bool -> Bool -> Bool) -> BDD -> BDD -> BDD           
apply oper x y = do { n <- x; m <- y; applyI oper n m}         

sameI n m =
  do { x1 <- node2 n
     ; y1 <- node2 m
     ; case (x1,y1) of 
         (Leaf n,Leaf m) -> return(n==m)
         (Node x lx hx,Node y ly hy) | x==y ->
            do { b1 <- sameI lx ly
               ; b2 <- sameI hx hy
               ; return(b1 && b2) }
         (_,_) -> return False }

same:: BDD -> BDD -> Bool         
same x y = i        
  where computation = do { m <- x; n <- y; sameI m n}
        (i,(next,tab1,tab2)) = MS.runState computation stateZero

solutionI n = 
  do { x <- node2 n
     ; case x of
         (Leaf True) -> return [[]]
         (Leaf False) -> return []
         (Node x lo hi) -> 
            do { xs <- solutionI lo
               ; ys <- solutionI hi
               ; return(map ((x,False):) xs ++ map ((x,True):) ys)}}

solution:: BDD -> [[(String, Bool)]]          
solution n = ans     
 where computation = do { x <- n; solutionI x}
       (ans,(next,tab1,tab2)) = MS.runState computation stateZero

-----------------------------------------------------
-- memoizing apply

applyJ:: (Bool -> Bool -> Bool)
         -> (Int -> Int -> MS.State Triple Int)
         -> (Int -> Int -> MS.State Triple Int)
applyJ oper call n m =
  do { x1 <- node2 n
     ; y1 <- node2 m
     ; case (x1,y1) of 
         (Leaf n,Leaf m) -> lift oper n m           -- Case 1 page 375
         (Node x lx hx,Node y ly hy) | x==y ->      -- Case 2 page 375
            do { lo <- call lx ly
               ; hi <- call hx hy
               ; (makeNode x lo hi) } 
         (Node x lx hx,Node y ly hy) | y>x ->       -- Case 3a page 375
            do { lo <- call lx m
               ; hi <- call hx m
               ; (makeNode x lo hi) } 
         (Node x lx hx,Node y ly hy) | y<x ->       -- Case 4a page 375
            do { lo <- call n ly
               ; hi <- call n hy
               ; (makeNode y lo hi) }               
         (Node x lx hx,Leaf _) ->                   -- Case 3b page 375
            do { lo <- call lx m
               ; hi <- call hx m
               ; (makeNode x lo hi) }
         (Leaf _,Node y ly hy) ->                   -- Case 4b page 375
            do { lo <- call n ly
               ; hi <- call n hy
               ; (makeNode y lo hi) }         
     }     
     
apply2 oper x y = 
   do { n <- x; m <- y
      ; is <- indexes
      ; let table = DM.fromList [ ((x,y),help x y) | x <- is, y <- is]
            help x y = case DM.lookup (x,y) table of
                         Just n -> n
                         Nothing -> applyJ oper help x y
      ; help n m} 


----------------------------------------
                      
m1 = node "x" true false
m2 = node "x" false true
m3 = node "y" m1 m2
m4 = node "z" (node "w" m3 m2) m3

ex614 = e
  where f = false
        t = true
        a = node "x3" f t
        b = node "x3" f t
        c = node "x2" f a
        d = node "x2" a b
        e = node "x1" c d

------------------------------------------------- 

