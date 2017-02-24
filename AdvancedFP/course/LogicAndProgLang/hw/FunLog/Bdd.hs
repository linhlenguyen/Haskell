{-# LANGUAGE  FlexibleContexts, FlexibleInstances, TypeSynonymInstances,
              MultiParamTypeClasses #-}
              
module Bdd where

import qualified MyMap as DM
import qualified Control.Monad.State as MS
import Data.IORef
import System.IO.Unsafe(unsafePerformIO)
-- import Control.Monad.Identity hiding (fix)
import Graphviz hiding (TreeF,shape)
import qualified Graphviz as GV

import SetsAsProp 
import Boolean
import Prop
import Minisat

-------------------------------------------------
-- TreeF is a F-structure that encodes the shape
-- of a Bdd tree.

data TreeF s i = Leaf Bool | Node s i i deriving (Eq,Ord)

showTreeF f (Leaf b) = "Leaf "++show b
showTreeF f (Node x y z) = "Node '"++f x++"' "++show y++" "++show z

instance (Show s,Show i) => Show(TreeF s i) where
  show s = showTreeF show s
  
instance Functor (TreeF s) where
  fmap f (Leaf b) = Leaf b
  fmap f (Node s x y) = Node s (f x) (f y)
  
----------------------------------------------------------------
-- Binary boolean Operators

data Binary = And | Or | Imply | Xor | Equal
  deriving (Eq,Ord,Show)

operate And x y = x && y
operate Or x y = x || y
operate Imply x y = imp x y
operate Xor x y = exOr x y
operate Equal x y = sameB x y

-------------------------------------------------

newtype Bdd s = Bdd (Int,TreeF s (Bdd s))

shapeOf (Bdd (n,shape)) = shape
index (Bdd (n,shape)) = n

instance Show (Bdd s) where
  show (Bdd (n,ref)) = "#"++show n

instance Eq (Bdd s) where
  (Bdd(x,y)) == (Bdd(a,b)) = a==x
  
instance Ord (Bdd s) where
  compare (Bdd(x,_)) (Bdd(y,_)) = compare x y
  
---------------------------------------------------------
-- displaying Bdd's

stringBdd:: Ord s => (s -> String) -> Bdd s -> String
stringBdd g bdd = (unlines (map f list))
  where (ptr,list) = reachList bdd
        f (k,a) | k==ptr = (" -> "++show k++" "++showTreeF g a)
        f (k,a)          = ("    "++show k++" "++showTreeF g a)
      
     
printBdd g bdd = putStrLn (stringBdd g bdd)

-- Uses Graphviz to draw a picture.

dagBdd:: Ord s => (s -> String) -> Bdd s -> IO ()
dagBdd g bdd = 
  do { let (ptr,list) = reachList bdd
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

reachList :: Ord s => Bdd s -> (Int, [(Int, TreeF s Int)])
reachList bdd = (i,DM.toList map)
   where (i,map) = reachSet bdd DM.empty

-- This function visits each node only once. It uses the
-- DM.Map to remember where it has already visited, as well
-- as to compute the answer. This makes the time linear in
-- the size of the Bdd DAG.
       
reachSet:: Ord s => Bdd s -> DM.Map Int (TreeF s Int) -> (Int,DM.Map Int (TreeF s Int))
reachSet (Bdd (i,_)) map | DM.member i map = (i,map)
reachSet (bdd@(Bdd(n,_))) map = f (shapeOf bdd)
 where f (Node s l r) = (n,DM.insert n (Node s i j) map2)
          where (i,map1) = reachSet l map
                (j,map2) = reachSet r map1
       f (Leaf x) = (n,DM.insert n (Leaf x) map)        
       
       
pureMemo1 f bdd = MS.evalState (g bdd) DM.empty where
  g bdd = memo (f g) bdd
            
memo f x = do { tab <- MS.get; try(DM.lookup (index x) tab)}
  where try (Just t) = return t
        try Nothing  = do { ans <- f x
                          ; tab2 <- MS.get
                          ; MS.put(DM.insert (index x) ans tab2)
                          ; return ans }          

paths :: Bdd t -> ([[(t, Bool)]], [[(t, Bool)]])
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

toCnf f xs = foldr andP TruthP (map (foldr orP AbsurdP . map f) xs)
toDnf f xs = foldr orP AbsurdP (map (foldr andP TruthP . map f) xs)

toLitPair (x,True) = LetterP x
toLitPair (x,False) = NotP(LetterP x)

dnfBdd x = toDnf (notP . toLitPair) (snd (paths x))
cnfBdd x = toCnf toLitPair (fst (paths x))

cnfFile file prop = 
  do { bdd <- prop2Bdd prop
     ; let (p1,p2) = paths bdd
     ; putClauses file (map (map toLitPair) p1)
     }

-------------------------------------------------------------------]
-- initializing the monad for doing Bdd's in the IO monad

type Hash s = DM.Map (TreeF s (Bdd s)) (Bdd s)
type Memo s = DM.Map (Binary,Bdd Int,Bdd Int) (Bdd Int)

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

findOrCreate :: TreeF Int (Bdd Int) -> IO (Bdd Int)
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
-- "oper" is any Binary Boolean operator (used on the Leaf's)

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

----------------------------------------------------------
-- 1 level-deep optimizing operations on Bdd that know 
-- about the special properties of trueBdd and falseBdd
-- and that call memoized apply if they are not present.

andBdd (Bdd (1,Leaf True)) x = return x
andBdd x (Bdd (1,Leaf True)) = return x
andBdd (Bdd (0,Leaf False)) x = return falseBdd
andBdd x (Bdd (0,Leaf False)) = return falseBdd
andBdd x y | x==y = return x
andBdd x y = memocall And (x,y)

orBdd (Bdd (1,Leaf True)) x = return trueBdd
orBdd x (Bdd (1,Leaf True)) = return trueBdd
orBdd (Bdd (0,Leaf False)) x = return x
orBdd x (Bdd (0,Leaf False)) = return x
orBdd x y | x==y = return x
orBdd x y = memocall Or (x,y)

notBdd (Bdd (0,Leaf False)) = return trueBdd
notBdd (Bdd (1,Leaf True)) = return falseBdd
notBdd x = memocall Xor (x,trueBdd)

implyBdd (Bdd (0,Leaf False)) _ = return trueBdd
implyBdd (Bdd (1,Leaf True)) x = return x
implyBdd x (Bdd (0,Leaf False)) = notBdd x
implyBdd x (Bdd (1,Leaf True)) = return trueBdd
implyBdd x y | x==y = return trueBdd
implyBdd x y = memocall Imply (x,y)

xorBdd (Bdd (1,Leaf True)) x = notBdd x
xorBdd x (Bdd (1,Leaf True)) = notBdd x
xorBdd (Bdd (0,Leaf False)) x = return x
xorBdd x (Bdd (0,Leaf False)) = return x
xorBdd x y | x==y = return falseBdd
xorBdd x y = memocall Xor (x,y)

equalBdd (Bdd (1,Leaf True)) x = return x
equalBdd x (Bdd (1,Leaf True)) = return x
equalBdd (Bdd (0,Leaf False)) x = notBdd x
equalBdd x (Bdd (0,Leaf False)) = notBdd x
equalBdd x y | x==y = return trueBdd
equalBdd x y = memocall Equal (x,y)

andLBdd [] = return trueBdd
andLBdd [x] = return x
andLBdd (x:xs) = do { ys <- andLBdd xs; andBdd x ys}

orLBdd [] = return falseBdd
orLBdd [x] = return x
orLBdd (x:xs) = do { ys <- orLBdd xs; orBdd x ys}

--------------------------------------  
-- Boolean instances For Bdd

instance TPD (Bdd n) where
  true = trueBdd
  false = falseBdd
  isTrue x = (x==trueBdd)
  isFalse x = (x==falseBdd)

instance BooleanM IO (Bdd Int) where
  conjM x y = andBdd x y
  disjM x y = orBdd x y
  negM x = notBdd x
  implyM x y = implyBdd x y
  xorM x y = xorBdd x y
  equalM x y = equalBdd x y

-----------------------------------------------
-- Turning Prop into a Bdd
-- we might try some optimization on the Prop level
-- i.e. use "opt" or "optimize" before transforming.

prop2Bdd :: Prop Int -> IO (Bdd Int)
prop2Bdd TruthP  = return trueBdd
prop2Bdd AbsurdP = return falseBdd
prop2Bdd (LetterP s) = findOrCreate(Node s falseBdd trueBdd)
prop2Bdd (AndP x y) =  do { a <- prop2Bdd x; b <- prop2Bdd y; andBdd a b}
prop2Bdd (OrP x y) = do { a <- prop2Bdd x; b <- prop2Bdd y; orBdd a b}
prop2Bdd (ImpliesP x y) = do { a <- prop2Bdd x; b <- prop2Bdd y; implyBdd a b}
prop2Bdd (NotP x) = do { a <- prop2Bdd x; notBdd a}
  
--------------------------------

p = LetterP

pp1 = (p 1) /\ ((p 2 ~> p 3) \/ p 4) ~> (p 5 /\ (NotP (p 0)))

pp2 = (p 2 ~> p 3) /\ ((p 2 ~> p 3) \/ p 4) ~> ((p 1 \/ p 5) /\ (NotP (p 0)))

set univ set = foldr acc AbsurdP univ
  where acc x prop | elem x set = orP (get x) prop
        acc x prop = prop
        (mapping,j) = initial univ 0
        get n = case lookup n mapping of
                  Just literals -> andL literals

formula univ x = foldr acc TruthP (fst(initial univ 0))
  where acc (y,ps) prop = andP (if x==y then (andL ps) else notP (andL ps))prop
        
    
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
     ; dagBdd show b
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
      
      
 