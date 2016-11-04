{-# LANGUAGE  FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}
module BddIO where

import Control.Monad.Identity hiding (fix)
import qualified Control.Monad.State as MS
import qualified Data.Map as DM
import qualified Data.Array as AR
import qualified Data.Set as DS
import Data.List(sortBy,insert)
import Prop
import SetsAsProp


-- import qualified FiniteSet as FS

import Graphviz hiding (Shape,shape)
import qualified Graphviz as GV

import Debug.Trace
import Data.IORef
import System.IO.Unsafe(unsafePerformIO)

--------------------------------------------

data Shape s i = Leaf Bool | Node s i i deriving (Eq,Ord)

showShape f (Leaf b) = "Leaf "++show b
showShape f (Node x y z) = "Node '"++f x++"' "++show y++" "++show z

instance (Show s,Show i) => Show(Shape s i) where
  show s = showShape show s
  
data BDD s = 
  BDD{ ptr:: Int 
     , table :: (AR.Array Int (Shape s Int)) }

(!) arr i = (AR.!) arr i

---------------------------------------------------- 
-- extract the shape of the root of the BDD tree

shape:: BDD s -> Shape s (BDD s)
shape (BDD n arr) = fmap g (arr ! n)  where g n = (BDD n arr)

-- extract the shape of a subtree inside the BDD
subShape :: BDD s -> Int -> Shape s Int
subShape (BDD _ arr) i = arr ! i

reachable :: BDD t -> DS.Set Int
reachable (b@(BDD n arr)) = reachShape n (subShape b n) (DS.insert 0 (DS.insert 1 DS.empty))
  where reachShape n (Leaf _) ans = DS.insert n ans
        reachShape n (Node s _ _) ans | DS.member n ans = ans
        reachShape n (Node s x y) ans = reachShape x (subShape b x) (reachShape y (subShape b y) (DS.insert n ans))

fetch k tab =   
   case DM.lookup k tab of
     Just z -> z
     Nothing -> error ("Bad call to 'fetch' "++show k++" "++show tab)
         
-----------------------------------------------
-- The approach above creates many intermediate BDD's
-- and repeats computation (perhaps) many times. We can
-- do better by memozing past computations. To do this
-- we will need to store some stuff in Tables (we will
-- need 3 different tables) 

data Quad s = Q Int
                (DM.Map (Shape s Int) Int)  -- index of structure
                (DM.Map Int (Shape s Int))  -- Structure of index i
                (DM.Map (Int,Int) Int)      -- memo table

-- We will thread the tables trough a computation
-- using a state monad.

type M s a = MS.StateT (Quad s) IO a

-- Operations in the M monad

next2 :: Ord s => M s Int
next2 = (MS.mapStateT f (return ()))
  where f comp = do { ((),(Q i m1 m2 m3)) <- comp
                    ; return(i,(Q (i+1) m1 m2 m3))}


find2 :: Ord s => Shape s Int -> M s (Maybe Int)
find2 x = MS.mapStateT f (return ())
  where f comp = do { ((),q@(Q i tab1 tab2 tab3)) <- comp
                    ; return(DM.lookup x tab1, q)}
 
fetch2 :: (Ord s,Show s) => Shape s Int -> M s Int
fetch2 x = MS.mapStateT f (return ())
  where f comp = do { ((),q@(Q i tab1 tab2 tab3)) <- comp
                    ; return(fetch x tab1,q) }


update2 :: Ord s => Int -> Shape s Int -> M s Int          
update2 n term =  MS.StateT f
  where f (Q i tab1 tab2 tab3) = return $
          (n,(Q i (DM.insert term n tab1)
                  (DM.insert n term tab2)
                  tab3))          

shape2:: (Ord s,Show s) => Int -> M s (Shape s Int)
shape2 n = MS.mapStateT f (return ())
  where f comp = do { ((),q@(Q i tab1 tab2 tab3)) <- comp
                    ; return(fetch n tab2,q)}


getTable:: M s (DM.Map (Int,Int) Int)   
getTable = MS.mapStateT f (return ())
  where f comp = do { ((),q@(Q i tab1 tab2 tab3)) <- comp
                    ; return(tab3,q)}

setTable:: (DM.Map (Int,Int) Int)  -> M s (DM.Map (Int,Int) Int)  
setTable new = MS.mapStateT f (return ())
  where f c = do { ((),q@(Q i tab1 tab2 tab3))<- c; return(tab3,Q i tab1 tab2 new) }

updateTable:: (Int,Int) -> Int -> M s ()
updateTable (x,y) ans = MS.mapStateT f (return ())
  where f c = do { ((),Q i tab1 tab2 tab3) <- c
                 ; return ((),Q i tab1 tab2 (DM.insert (x,y) ans tab3))}

initTab:: Ord s => (DM.Map (Shape s Int) Int)
initTab = DM.insert (Leaf False) 0 (DM.insert (Leaf True) 1 DM.empty)

initTab2:: Ord s => (DM.Map Int (Shape s Int))
initTab2 = DM.insert 0 (Leaf False) (DM.insert 1 (Leaf True) DM.empty)
 
-- The initial state
stateZero :: Ord s => Quad s
stateZero = (Q 2 tab1 tab2 tab3)
  where tab1 = initTab -- DM.insert (Leaf False) 0 (DM.insert (Leaf True) 1 DM.empty)
        tab2 = initTab2
        tab3 = DM.empty

-- Once a table is created, any entry can be the root of a BDD.
-- So we create a State computation, run it to get the tables.
-- Given an index in the table extract an answer
-- The function "close" does this, and returns a function
-- that can make BDDs from the internal tables.


close:: (Ord s,Show s) => M s a -> IO (a,Int -> BDD s)
close comp = do ((ptr,f),quad) <- MS.runStateT (extract comp) stateZero
                return(ptr,f)

                                            
extract:: (Ord s,Show s) => M s a -> M s (a,Int -> BDD s)
extract comp = do { ptr <- comp
                 ; (Q next tab1 tab2 tab3) <- MS.get
                 ; return(ptr,f tab2)}
  where f tab2 n = (BDD (rename n) table)
           where reachable = zip (DS.toList (reachShape n (fetch n tab2) (DS.fromList [0,1]))) [0 ..]
                 len = (length reachable) - 1
                 rename x = case lookup x reachable of
                              Just n -> n
                 table = AR.array (0,len) (DM.toList(extract2 tab2 rename n (fetch n tab2) initTab2))
                 reachShape n (Leaf _) ans = DS.insert n ans
                 reachShape n (Node s _ _) ans | DS.member n ans = ans
                 reachShape n (Node s x y) ans = reachShape x (fetch x tab2) 
                                                (reachShape y (fetch y tab2) 
                                                              (DS.insert n ans))



addOne k v map = 
  case DM.lookup k map of
    Just u -> map
    Nothing -> DM.insert k v map
    
extract2 tab rename n (Leaf x) ans = addOne (rename n) (Leaf x) ans
extract2 tab rename n (Node s x y) ans = 
     extract2 tab rename x (fetch x tab) $
     extract2 tab rename y (fetch y tab) $
       (addOne (rename n) (Node s (rename x) (rename y)) ans)


-- Create a node, if its not already present
makeNode :: (Ord s,Show s) => s -> Int -> Int -> M s Int
makeNode s a b | a==b = return a           -- CASE 1 page 373
makeNode s a b = do {test <- find2 term; findOrCreate test }
  where term = Node s a b 
        findOrCreate (Just n) = return n   -- CASE 2 page 373
        findOrCreate Nothing  =            -- CASE 3 page 373
           do { n <- next2; update2 n term; return n }
        

withSavedTable:: M s a -> M s a
withSavedTable comp = 
  do { save <- getTable
     ; setTable (DM.empty)
     ; ans <- comp
     ; setTable save
     ; return ans }


-- Now to make a BDD we use the tables to hashTabze all
-- sub BDDs that we make in the process.
     
fixApply:: (Ord s,Show s) => (Bool -> Bool -> Bool) ->
         (Int -> M s(Shape s Int)) ->
         (Int -> M s(Shape s Int)) ->
         ((Int,Int) -> M s Int) ->
         ((Int,Int) -> M s Int)
         
fixApply oper f1 f2 call (x1,x2) = do { i <- f1 x1; j <- f2 x2; f (i,j)} where
   varLess x (Node y _ _) = x<y
   varLess x (Leaf _) = True
   f(Leaf n,Leaf m) = fetch2 (Leaf (oper n m)) -- Case 1 page 375      
   f(Node x lx hx,Node y ly hy) | x==y =       -- Case 2 page 375
       do { lo <- call (lx,ly)
          ; hi <- call (hx,hy)
          ; makeNode x lo hi } 
   f(Node x lx hx,term) | varLess x term =     -- Case 3 page 375
       do { lo <- call (lx,x2)
          ; hi <- call (hx,x2)
          ; makeNode x lo hi } 
   f(term,Node y ly hy) | varLess y term =     -- Case 4 page 375
       do { lo <- call (x1,ly)
          ; hi <- call (x1,hy)
          ; makeNode y lo hi }                                   
  

applyM fi fj oper (i,j) = 
  do { tab3 <- getTable
     ; case DM.lookup (i,j) tab3 of
         Just n -> return n
         Nothing -> do { ans <- fixApply oper fi fj (applyM fi fj oper) (i,j)
                       ; updateTable (i,j) ans
                       ; return ans }}  

p2Pair oper x y = 
  do { xi <- p2M x; yi <- p2M y
     ; withSavedTable
      (applyM shape2 shape2 oper (xi,yi)) }


write:: String -> M s ()
write s = MS.lift(putStrLn s)
                      
------------------------------------------------------------
-- Are 2 BBDs the same function?
same n m =
  case (shape n,shape m) of 
    (Leaf n,Leaf m) -> n==m
    (Node x lx hx,Node y ly hy) | x==y ->
            same lx ly && same hx hy
    (_,_) -> False   

-- A list of all the satisfying solutions
solutions n = 
  case (shape n) of
    (Leaf True) -> [[]]
    (Leaf False) -> []
    (Node x lo hi) -> 
      let xs = solutions lo
          ys = solutions hi
      in map ((x,False):) xs ++ map ((x,True):) ys 
      
tautology (BDD 1 _) = True
tautology _ = False

unsat (BDD 0 _) = True
unsat _ = False


---------------------------------------------------------------
-- Turning Prop into (M s Int) with no intermediate BDDs
-- Important that the implict tables in the Monad only
-- grow and aren't overwritten.


p2M :: (Ord s,Show s) => Prop s -> M s Int
p2M (LetterP s) = do { t <- p2M TruthP; f <- p2M AbsurdP; makeNode s f t}
p2M AbsurdP = fetch2(Leaf False)
p2M TruthP = fetch2(Leaf True)
p2M (AndP x y) = p2Pair (&&) x y
p2M (OrP x y)  = p2Pair (||) x y
p2M (ImpliesP x y) = p2Pair imp x y
p2M (NotP x) = p2Pair exOr x TruthP



drawBddIO:: Prop Int -> IO ()
drawBddIO x = do { (i,f) <- close (p2M x)
                 ; pnG(f i)
                 ; return ()}

  
class Functor f => Sequence f where
  swap:: Monad m => f(m a) -> m(f a)
instance Sequence [] where
  swap = sequence
instance Sequence Maybe where
  swap Nothing = return Nothing
  swap (Just m) = do { x <- m; return(Just x)}
 
instance Simple s =>  Graph (BDD s) where 
  nodes (BDD p tab) = ([(0,"F",Red,shape 0),(1,"T",Green,shape 1)]++ foldr f [] pairs)
    where shape n | n==p = Doublecircle
          shape n = Oval
          pairs = AR.assocs tab
          f (k,Node v i j) ans | k==p = (k,"P"++simple v,None,shape k):ans
          f (k,Node v i j) ans = (k,"P"++simple v,None,shape k):ans
          f (k,Leaf True) ans = ans
          f (k,Leaf False) ans = ans
  edges (BDD p tab) = concat(fmap f (AR.assocs tab))
    where f (k,Node var i j) = [(k,i,"F",Red),(k,j,"T",Green)]
          f (k,Leaf _) = []
          
----------------------------------------------
-- Class instances for Shape and BDD

instance Functor (Shape s) where
  fmap f (Leaf b) = Leaf b
  fmap f (Node s x y) = Node s (f x) (f y)

showBDD :: Show s => BDD s -> String
showBDD (x@(BDD i arr)) = unlines (reverse(map f pairs))
  where good = reachable x
        ok (k,a) = True -- DS.member k good
        f (k,a) | k==i = (" -> "++show k++" "++show a)
        f (k,a)        = ("    "++show k++" "++show a)
        pairs = AR.assocs arr -- filter ok (AR.assocs arr)
                -- ([(0,Leaf fl),(1,Leaf tr)]++AR.assocs arr)

instance Show s => Show (BDD s) where  show x = showBDD x

p = LetterP

pp1 = (p 1) /\ ((p 2 ~> p 3) \/ p 4) ~> (p 5 /\ (NotP (p 0)))

pp2 = (p 2 ~> p 3) /\ ((p 2 ~> p 3) \/ p 4) ~> ((p 1 \/ p 5) /\ (NotP (p 0)))

------------------------------------------------

run comp = MS.runStateT comp stateZero


comp:: M Int ()
comp =
  do { write (show (initial "abcd" 0))
     ; let prop = (set "abcd" "ad")
     ; subset <- p2M prop
     ; x <- p2M (formula "abcd" 'a')
     ; write("\nsubset = "++show subset) 
     ; write("\nx1 = "++show x)
     ; b <- (membership x subset)
     ; write("\nin the set = "++show b)
     ; (_,f) <- extract(return ())
     ; let bdd = f subset
     ; let x1 = f x
     ; MS.lift(pnG x1)
     ; return ()
     }

main = run comp

-----------------------------------------------

           
set univ set = foldr acc AbsurdP univ
  where acc x prop | elem x set = orB (get x) prop
        acc x prop = prop
        (mapping,j) = initial univ 0
        get n = case lookup n mapping of
                  Just literals -> andL literals

formula univ x = foldr acc TruthP (fst(initial univ 0))
  where acc (y,ps) prop = andB (if x==y then (andL ps) else notB (andL ps))prop
        
                 
membership i x = do { b <- conjBDD i x; return(b == trueBDD)}

trueBDD = 1
falseDBDD = 0


conjBDD xi yi = withSavedTable (applyM shape2 shape2 (&&) (xi,yi))
disjBDD xi yi =  withSavedTable (applyM shape2 shape2 (||) (xi,yi))
negBDD  xi = withSavedTable (applyM shape2 shape2 (exOr) (xi,trueBDD))
 
----------------------------------------------------------------
-- Binary boolean Operators

data Binary = And | Or | Imply | Xor | Equal
  deriving (Eq,Ord,Show)

operate And x y = x && y
operate Or x y = x || y
operate Imply x y = imp x y
operate Xor x y = exOr x y
operate Equal x y = sameB x y

imp False x = True
imp True x = x

exOr True False = True
exOr False True = True
exOr _ _ = False

sameB True True = True
sameB False False = True
sameB _ _ = False

-------------------------------------------------

newtype Bdd s = Bdd (Int,Shape s (Bdd s))


instance Show (Bdd s) where
  show (Bdd (n,ref)) = "#"++show n

instance Eq (Bdd s) where
  (Bdd(x,y)) == (Bdd(a,b)) = a==x
  
instance Ord (Bdd s) where
  compare (Bdd(x,_)) (Bdd(y,_)) = compare x y
  
---------------------------------------------------------
-- displaying Bdd's

stringBdd:: Ord s => (s -> String) -> Bdd s -> IO String
stringBdd g bdd = 
  do { (ptr,list) <- reachList bdd
     ; let f (k,a) | k==ptr = (" -> "++show k++" "++showShape g a)
           f (k,a)          = ("    "++show k++" "++showShape g a)
     ; return(unlines (map f list))
     }
     
printBdd g bdd = do { s <- stringBdd g bdd; putStrLn s}

dagBdd:: Ord s => (s -> String) -> Bdd s -> IO ()
dagBdd g bdd = 
  do { (ptr,list) <- reachList bdd
     ; let label (Leaf True) = "T"
           label (Leaf False) = "F"
           label (Node s l r) = "p"++g s
           color (i,n) | i==ptr = Blue 
           color (_,Leaf False) = Red
           color (_,Leaf True) = Green
           color (_,_) = None
           node (i,shape) = (i,label shape,color (i,shape),Oval)
           edge (i,Node s j k) = [(i,j,"",Red),(i,k,"",Green)]
           edge (i,Leaf b) = []
           gr = Gr (map node list) ( {- (-1,ptr,"",None): -} (concat (map edge list)))
     ; pnG gr
     ; return ()
     }

-------------------------------------------------------------
-- operations on Bdd's

shapeOf (Bdd (n,shape)) = shape
index (Bdd (n,shape)) = n

reachList bdd = do { (i,map) <- reachSet bdd DM.empty; return(i,DM.toList map)} 

-- This function visits each node only once. It uses the
-- DM.Map to remember where it has already visited, as well
-- as to compute the answer. This makes the time linear in
-- the size of the Bdd DAG.

reachSet:: Ord s => Bdd s -> DM.Map Int (Shape s Int) -> IO(Int,DM.Map Int (Shape s Int))
reachSet (Bdd (i,_)) map | DM.member i map = return (i,map)
reachSet (bdd@(Bdd(n,_))) map = f (shapeOf bdd)
 where f (Node s l r) = 
          do { (i,map1) <- reachSet l map
             ; (j,map2) <- reachSet r map1
             ; return(n,DM.insert n (Node s i j) map2)}
       f (Leaf x) = return(n,DM.insert n (Leaf x) map) 
       
       
pureMemo1 f bdd = MS.evalState (g bdd) DM.empty where
  g bdd = memo (f g) bdd
            
memo f x = do { tab <- MS.get; try(DM.lookup (index x) tab)}
  where try (Just t) = return t
        try Nothing  = do { ans <- f x
                          ; tab2 <- MS.get
                          ; MS.put(DM.insert (index x) ans tab2)
                          ; return ans }          

paths bdd = pureMemo1 f bdd
  where f rcall (Bdd (n,Leaf  True)) = return([],[[]])
        f rcall (Bdd (n,Leaf False)) = return([[]],[])
        f rcall (Bdd (n,Node s fdag tdag)) = 
          do { (fs1,ts1) <- rcall fdag
             ; (fs2,ts2) <- rcall tdag
             ; let true xs = map ((s,True):) xs
                   false xs = map ((s,False):) xs
             ; return(false fs1 ++ true fs2,false ts1++ true ts2)}

solution x = snd(paths x)

toCnf f xs = foldr andB TruthP (map (foldr orB AbsurdP . map f) xs)
toDnf f xs = foldr orB AbsurdP (map (foldr andB TruthP . map f) xs)
toLitPair (x,True) = LetterP x
toLitPair (x,False) = NotP(LetterP x)
dnfBdd x = toDnf (notB . toLitPair) (snd (paths x))
cnfBdd x = toCnf toLitPair (fst (paths x))


-------------------------------------------------------------------]
-- initializing the monad for doing Bdd's

type Hash s = DM.Map (Shape s (Bdd s)) (Bdd s)
type Memo s = DM.Map (Binary,Bdd Int,Bdd Int) (Bdd Int)

count :: IORef Int 
count = unsafePerformIO(newIORef 0)

-- next = do { n <- readIORef count; writeIORef count (n+1); return n}

(falseBdd,trueBdd,hashTab,memoI,next,internals) = unsafePerformIO comp
  where comp = do { let i = 0
                        j = 1
                  ; count <- newIORef (j+1)
                  ; memoI <- newIORef (DM.empty:: Memo Int)  
                  ; let f = Bdd (i,Leaf False)
                        t = Bdd (j,Leaf True)
                        tab2 = DM.insert (Leaf False) f (DM.empty:: Hash Int)
                        tab3 = DM.insert (Leaf  True) t tab2
                        next = do { n <- readIORef count; writeIORef count (n+1); return n}
                  ; hashTab <- newIORef tab3
                  ; let stat = do { n <- readIORef count
                                  ; hash <- readIORef hashTab
                                  ; memo <- readIORef memoI
                                  ; return(n,hash,memo)}
                  ; return(f,t,hashTab,memoI,next,stat)
                  }                  

stat = do { (n,hash,memo) <- internals
          ; putStrLn ("\nCells created to date: "++show n)
          ; putStrLn (  "Hash table size:       "++show (DM.size hash))
          ; putStrLn (  "Memo table size:       "++show (DM.size memo))
          }
          
          
----------------------------------------------------------
-- Creating Bdd's

findOrCreate :: Shape Int (Bdd Int) -> IO (Bdd Int)
findOrCreate (Leaf  True) = return trueBdd
findOrCreate (Leaf False) = return falseBdd
findOrCreate (Node s l r) | l==r = return l
findOrCreate (shape@(Node s l r)) = 
  do { tab <- readIORef hashTab
     ; case DM.lookup shape tab of
         Just bdd -> return bdd
         Nothing -> do { i <- next
                       ; let shape = Node s l r
                             ans = Bdd(i,shape)
                       ; writeIORef hashTab (DM.insert shape ans tab)
                       ; return ans } }

-- implementation of an algorithm described in 
-- Huth and Ryan's "Logic in Computer Science" page 375 
-- "oper" is any Bibary Boolean operator

fixApplyM (fs@(shape1,shape2,findOrCreate,memocall)) oper (x1,x2) 
       = do { i <- shape1 x1; j <- shape2 x2; f (i,j)} where
   varLess x (Node y _ _) = x<y
   varLess x (Leaf _) = True
   f(Leaf n,Leaf m) = findOrCreate (Leaf (operate oper n m)) -- Case 1 page 375      
   f(Node x lx hx,Node y ly hy) | x==y =             -- Case 2 page 375
       do { lo <- memocall (lx,ly)
          ; hi <- memocall (hx,hy)
          ; findOrCreate (Node x lo hi) } 
   f(Node x lx hx,term) | varLess x term =           -- Case 3 page 375
       do { lo <- memocall (lx,x2)
          ; hi <- memocall (hx,x2)
          ; findOrCreate (Node x lo hi) } 
   f(term,Node y ly hy) | varLess y term =           -- Case 4 page 375
       do { lo <- memocall (x1,ly)
          ; hi <- memocall (x1,hy)
          ; findOrCreate (Node y lo hi) }                                   

-- This is a memoized version of fixApplyM

memocall :: Binary -> (Bdd Int, Bdd Int) -> IO (Bdd Int)                                                    
memocall oper (x,y) = 
  do { table <- readIORef memoI
     ; case DM.lookup (oper,x,y) table of
         Just n -> return n
         Nothing -> 
            do { ans <- fixApplyM (return . shapeOf,return . shapeOf,findOrCreate,memocall oper) oper (x,y)
               ; writeIORef memoI (DM.insert (oper,x,y) ans table)
               ; return ans }
     }               

apply :: Binary -> Prop Int -> Prop Int -> IO (Bdd Int)                                  
apply oper x y = 
   do { a <- prop2Bdd x; b <- prop2Bdd y; memocall oper (a,b)}      
 
-- Turn a Prop into a Bdd

prop2Bdd :: Prop Int -> IO (Bdd Int)
prop2Bdd TruthP  = return trueBdd
prop2Bdd AbsurdP = return falseBdd
prop2Bdd (LetterP s) = findOrCreate(Node s falseBdd trueBdd)
prop2Bdd (AndP x y) = apply And x y
prop2Bdd (OrP x y) = apply Or x y
prop2Bdd (ImpliesP x y) = apply Imply x y
prop2Bdd (NotP x) = apply Xor x TruthP

andBdd x y = memocall And (x,y)
orBdd x y = memocall Or (x,y)
notBdd x = memocall Xor (x,trueBdd)
implyBdd x y = memocall Imply (x,y)
xorBdd x y = memocall Xor (x,y)
equalBdd x y = memocall Equal (x,y)

andLBdd [] = return trueBdd
andLBdd [x] = return x
andLBdd (x:xs) = do { ys <- andLBdd xs; andBdd x ys}

orLBdd [] = return falseBdd
orLBdd [x] = return x
orLBdd (x:xs) = do { ys <- orLBdd xs; orBdd x ys}
  
--------------------------------

memberBdd i x = do { b <- memocall And (i,x); return(b == i)}

test3 =
  do { b <- prop2Bdd pp2
     ; printBdd show b
     ; putStrLn (show (fst(initial "abcd" 0)))
     ; let prop = (set "abcd" "ad")
     ; subset <- prop2Bdd prop
     ; x <- prop2Bdd (formula "abcd" 'c')
     ; printBdd show subset
     ; printBdd show x
     ; conj <- memocall And (subset ,x)
     ; printBdd show conj
     ; dagBdd show subset
     ; c <- (memberBdd x subset)
     ; putStrLn("\nin the set = "++show c)
     ; stat
     ; return subset
     }


g1 = Gr [(0,"s0",None,GV.Oval) 
        ,(1,"s1",None,GV.Oval)
        ,(2,"s2",None,GV.Oval)
        ,(3,"s3",None,GV.Oval)
        ] 
        [(0,1,"",Red),(0,2,"",Red)
        ,(1,0,"",Red),(1,2,"",Red),(2,2,"",Red),(1,3,"",Red),(3,3,"",Red)
        ]
        
initGr (Gr xs ys) = (univ,mapping,fixer,prop,Gr (map h xs) ys)
  where f (id,name,color,shape) = id
        g (from,to,name,color) = (from,to)
        (mapping,j) = initial univ 0
        univ = map f xs
        (fixer,prop,delta) = trans univ (map g ys) 0
        h (i,name,color,shape) = (i,name++"="++show(get id i)++" aka "++show(get hh i),color,shape)
        get hh i = case lookup i mapping of
                     Just xs -> andL (map hh xs)
                     Nothing -> AbsurdP
        hh (LetterP n) = LetterP(n+delta)
        hh (NotP x) = NotP(hh x)
        
(u1,tab1,fix,prop1,g2) = initGr g1
bdd1 = unsafePerformIO (prop2Bdd prop1)
tpaths = snd(paths bdd1)

subi xs = unsafePerformIO(prop2Bdd (subset u1 xs))

sub1 = subi [0]
sub2 = subi [2,3]
sub3 = subi [3]
sub4 = subi [1,2,3]

step sub = 
  do { bdd2 <- andBdd bdd1 sub
     ; truepaths <- mapM (prop2Bdd . fix) (snd (paths bdd2))
     ; orLBdd (sub:truepaths) }
     
closure p0 = 
   do { p1 <- step p0
      ; printBdd show p1
      ; if p1==p0 then return p0 else closure p1 }
 