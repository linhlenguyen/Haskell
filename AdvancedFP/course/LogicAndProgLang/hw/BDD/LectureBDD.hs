{-# LANGUAGE  FlexibleContexts, FlexibleInstances, TypeSynonymInstances #-}
module LectureBDD where

import Graphviz hiding (Shape)
import qualified Graphviz as GV
import System.Cmd(system)
import qualified Data.Array as AR
import Data.List(sort,nub)
import Data.Maybe

-- These are for pretty printing
import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ(Doc,text,int,(<>),(<+>),($$),($+$),render)

import Prop (Prop(..),(/\),(\/),(~>),letters,opt,optimize,cnf,dnf,andL,orL)
import SetsAsProp
import Soduko(alls)
import FiniteSetNotes(queenProp)
import FastBDD 

{-
boolean functions and their sizes
1) terms,  p1
2) tables
4) BDDTree (strutural similarity to proof by refutation)
3) BDDs

Anding two BDDTrees (replace true)
Oring two BDDTRees (replace False)

Combining common subexpressions
Three rules
  1) One node where both children are the same
  2) Two nodes with identical children
  3) Create a new node
  
mechanizing this. What do we need to know?
Two maps 1) Struct -> Node
         2) Node -> Struct
  

Order of variables from root to leaves matters
How do we enforce order
  1) Provide only basic BDDs, which are always in order
  2) Provide limited ways to combine, that always maintain order
  
Examples
True 
False
variables

Order presering transformations -- Not
  
Merging two variables (by using order)
Merging two arbitray terms
        Repeated sub problems, Dynamic programming
        memoizing results

Finding solutions (paths from root to True)
Is it a taulogy
is it unsatisfiable

Are two BDDs the same function.



-}


--------------------------------------
-- functions for displaying things

png :: (Show s) => BDD s -> IO (BDD s)
tab:: Prop Int -> IO () 
-- toPng :: (LabeledTree t) => t -> IO GHC.IO.Exception.ExitCode
-- p2b :: Ord a => Prop a -> BDD a
t2b:: (Ord s,Show s) => BDDTree s -> BDD s
t2b = treeToBdd 

--------------------------------------------------
-- p:: Int -> Prop Int
p n = LetterP n

p1 = (p 1) /\ ((p 2 ~> p 3) \/ p 4) ~> (p 5 /\ (NotP (p 0)))

p2 = (p 2 ~> p 3) /\ ((p 2 ~> p 3) \/ p 4) ~> ((p 1 \/ p 5) /\ (NotP (p 0)))


-----------------------------------------
-- The result of the soduko and queens problem

soduko = foldr AndP TruthP alls
sbdd = (p2b soduko)
sol1 = (solution sbdd) 

queen = foldr AndP TruthP queenProp
qbdd = p2b queen


--------------------------------------
-- Binary Decision Diagram Trees

data BDDTree s = In (Shape s (BDDTree s))  

leaf s = In(Leaf s)
node s x y = In(Node s x y)

treeM (In (Leaf b)) = fetch2 (Leaf b)
treeM (In (Node s x y)) =
  do { i <- treeM x
     ; j <- treeM y
     ; makeNode s i j}

treeToBdd x = f i
  where (i,f) = close(treeM x)     

-- Page 373 Figure 6.14
t1 = e 
f = leaf False
t = leaf True
a = node "x3" f t
b = node "x3" f t
c = node "x2" f a
d = node "x2" a b
e = node "x1" c d
        
b1 = treeToBdd t1

-- Page 375 Figure 6.15

t2 = r1
r5 = leaf False
r6 = leaf True
r4 = node "x4" r5 r6
r3 = node "x3" r4 r6
r2 = node "x2" r4 r3
r1 = node "x1" r2 r3
        
b2 = treeToBdd t2        

t3 = s1
s4 = leaf False
s5 = leaf True
s3 = node "S3 x4" s4 s5
s2 = node "S2 x3" s3 s5
s1 = node "S1 x1" s3 s2

badt3 = node "x5" s3 s2
        
b3 = treeToBdd t3        
        
        
x1 = conj (disj x y) (neg z)  
x2 = conj (disj x y) (conj (disj x y) (conj x (neg z)))


        
--------------------------------------------------------
-- Displaying BDD with Dot

-- dotBDD :: Show s => BDD s -> [String]
dotBDD (BDD i x y arr) = "digraph tree {\n"++ unlines (reverse(map f pairs)) ++ "\n}"
 where f (k,Leaf b)     = (show k++" [label="++show b++"];")
       f (k,Node x i j) = (show k++" [label="++show x++"];" ++ 
                           "\n"++show k++" -> "++show i++" [color=red];"++
                           "\n"++show k++" -> "++show j++" [color=green];")
       pairs = [(0,Leaf x),(1,Leaf y)]++AR.assocs arr
       

dotFile t = writeFile "tree.dot" (dotBDD t)

png t =
  do { dotFile t
     ; system "dot -Tpng tree.dot > tree.png"
     ; system "explorer tree.png "
     ; return t
     }



--------------------------------------------
-- instances of LabeledTree

pptree :: Show s => BDDTree s -> Doc
pptree (In(Leaf True)) = text "True"
pptree (In(Leaf False)) = text "False"
pptree (In(Node s x y)) = PP.parens(PP.sep[text("Node "++show s),pptree x,pptree y])

instance Show s => Show (BDDTree s) where
  show x = render(pptree x)
  
instance Tree (BDDTree a) where
  subtrees (In(Leaf x))   = []
  subtrees (In(Node s l r))  = [(l,Red),(r,Green)]
  
instance LabeledTree (BDDTree String) where
  label (In(Leaf x))   = show x
  label (In(Node s l r))  = s

instance Show s => Tree (Prop s) where
  subtrees (AndP x y) = [(x,None),(y,None)]
  subtrees (OrP x y) = [(x,None),(y,None)] 
  subtrees (NotP x) = [(x,None)]
  subtrees (ImpliesP x y) = [(x,None),(y,None)] 
  subtrees _ = []
  
instance (Show s) => LabeledTree (Prop s) where
  label (LetterP x)   = "P"++show x
  label TruthP = "T"
  label AbsurdP = "F"
  label (AndP x y) = "And"
  label (OrP x y) = "Or"
  label (ImpliesP x y) = "Implies"
  label (NotP x) = "not"
  

-----------------------------------------------------
eval :: Prop Int -> [Bool] -> Bool
eval (AndP x y) assign     = (eval x assign) && (eval y assign)   
eval (OrP x y) assign      = (eval x assign) || (eval y assign)   
eval (ImpliesP x y) assign = imply (eval x assign) (eval y assign)  
  where imply False _ = True
        imply True x = x
eval (NotP x) assign       = not (eval x assign)
eval TruthP assign         = True
eval AbsurdP assign        = False
eval (LetterP x) assign    =  assign !! x



assignments :: Int -> [[Bool]]    
assignments 0 = [[]]
assignments n = map (True :) tail ++ map (False :) tail
  where tail = assignments (n-1)

pad n s = take n (s ++ repeat ' ')

tab p = putStrLn (unlines(g vs : map f xs))
  where vs = sort(letters p)
        xs = assignments (maximum vs + 1)
        f xs  = g xs ++ "| "++ show (eval p xs)
        g [] = ""
        g (x:xs) = pad 5(show x)++" "++g xs
        
        
------------------------------------------------------

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
bdd1 = p2b prop1

sub1 = subset u1 [0]
sub2 = subset u1 [2,3]
sub3 = subset u1 [3]
sub4 = subset u1 [1,2,3]

(m1,i) = initial [0,1,2,3] 0
(m2,j) = initial [0,1,2,3] i

step sub = orL (sub : (map fix (solution (conj bdd1 (p2b sub)))))

closure sub = if same p0 (p2b p1) then sub else closure p1
  where p0 = p2b sub
        p1 = (step sub)
        
ans sub = p2b(closure sub)


eq:: Prop Int        
eq = andL [ImpliesP (p 3) (p 1)
          ,ImpliesP (p 1) (p 3)
          , ImpliesP (p 4) (p 2)
          ,ImpliesP (p 2) (p 4)]
        