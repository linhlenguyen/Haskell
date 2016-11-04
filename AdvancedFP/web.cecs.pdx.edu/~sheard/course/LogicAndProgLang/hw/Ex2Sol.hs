module Ex2 where

import SimpleProp


import Data.List(nub,sort,sortBy)
import Test.QuickCheck hiding(Prop(..))
import qualified Data.Map as DM



andL xs = foldr andOpt TruthP (sortBy compare xs)
andB x y = foldr andOpt TruthP (sortBy compare (collect (AndP x y)))
  where collect (AndP x y) = collect x ++ collect y
        collect x = [x]
        
andOpt TruthP x = x
andOpt x TruthP = x
andOpt AbsurdP x = AbsurdP
andOpt x AbsurdP = AbsurdP
andOpt x y | x==y = x
andOpt x (w@(AndP y z)) | x==y = w
andOpt x y = AndP x y

orL xs = foldr orOpt AbsurdP (sortBy compare xs)
orB x y = foldr orOpt AbsurdP (sortBy compare (collect (OrP x y)))
  where collect (OrP x y) = collect x ++ collect y
        collect x = [x]
        
orOpt TruthP x = TruthP
orOpt x TruthP = TruthP
orOpt AbsurdP x = x
orOpt x AbsurdP = x
orOpt x y | x==y = x
orOpt x (w@(OrP y z)) | x==y = w
orOpt x y = OrP x y

notB TruthP = AbsurdP
notB AbsurdP = TruthP
notB (NotP x) = x
notB x = NotP x

implyB AbsurdP _ = TruthP
implyB TruthP x = x
implyB x AbsurdP = notB x
implyB x TruthP = TruthP
implyB x y | x==y = TruthP
implyB x y = ImpliesP x y               

-- opt all sub terms and then combine with the (op)B functions
-- This is a bottom up taversal strategy

opt AbsurdP = AbsurdP
opt TruthP = TruthP
opt (LetterP x) = LetterP x
opt (AndP x y) = andB (opt x) (opt y)
opt (OrP x y) = orB (opt x) (opt y)
opt (ImpliesP x y) = implyB (opt x) (opt y)


----------------------------------------------------
-- The function opt uses strictly syntactic information
-- about a term. Below we try and use some semantic 
-- by tracking the possible values of variables in a table.
-- Add information about the value of variables 
-- in a term, assuming the term evaluates to True

assume x tab = 
  case x of
    (LetterP n) -> DM.insert n TruthP tab
    (AndP x y) -> assume x (assume y tab) 
    (NotP x) -> deny x tab
    other -> tab

-- add information about the value of variables 
-- in a term, assuming the term evaluates to False

deny x tab =
  case x of
    (LetterP n) -> DM.insert n AbsurdP tab
    (NotP x) -> assume x tab
    (OrP x y) -> deny x (deny y tab)
    other -> tab


-- Use this semantic information optimize all sub terms 
-- and then combine them with the (op)B (syntactic) functions

semanticTrans tab AbsurdP = AbsurdP
semanticTrans tab TruthP = TruthP
semanticTrans tab (LetterP x) =
  case DM.lookup x tab of
    Just e -> e
    Nothing -> LetterP x
semanticTrans tab (AndP x y) = andB (semanticTrans (assume y tab) x) (semanticTrans (assume x tab) y)
semanticTrans tab (OrP x y) = orB (semanticTrans (deny y tab) x) (semanticTrans (deny x tab) y)
semanticTrans tab (ImpliesP x y) = implyB (semanticTrans tab x) (semanticTrans (assume x tab) y)
semanticTrans tab (NotP x) = notB (semanticTrans tab x)

-- it s OK to add constraints to simplify
-- e.g.   simplify:: Eq n => Prop n -> Prop n

simplify:: Ord n => Prop n -> Prop n
simplify x = semanticTrans DM.empty x


------------------------------------------------------------------
-- Ord instances. Orders lists of Prop such that 
-- AbsurdP < TruthP < (LetterP and Not LetterP with same var) < AndP < OrP < NotP
-- sortBy orderProp [p3,p1 \/ p5,T,p4,~p3,p4,F,~(p4 /\ p5),p2 /\ p7]  -->  
--                  [F,T,p3,~p3,p4,p4,p2 /\ p7,p1 \/ p5,~(p4 /\ p5)]
-- This moves terms that trigger rules like (x /\ x) == x to be
-- adjacent in a sorted list.


instance Ord t => Ord (Prop t) where
  compare = orderProp

orderProp :: Ord t => Prop t -> Prop t -> Ordering
orderProp AbsurdP AbsurdP = EQ
orderProp AbsurdP _ = LT
orderProp _ AbsurdP = GT
orderProp TruthP TruthP = EQ
orderProp TruthP _ = LT
orderProp _ TruthP = GT
orderProp (LetterP n) (LetterP m) = compare n m
orderProp (NotP (LetterP n)) (NotP(LetterP m)) = compare n m
orderProp (LetterP n) (NotP(LetterP m)) =
   case compare n m of
     EQ -> LT
     other -> other
orderProp (NotP(LetterP m)) (LetterP n)  =
   case compare m n of
     EQ -> GT
     other -> other    
orderProp (LetterP _) _ = LT  
orderProp _ (LetterP _) = GT 
orderProp (NotP(LetterP _)) _ = LT  
orderProp _ (NotP(LetterP _)) = GT 
orderProp (AndP x xs) (AndP y ys) = 
 case orderProp x y of
   EQ -> orderProp xs ys
   other -> other
orderProp (AndP _ _) _ = LT
orderProp _ (AndP _ _) = GT 
orderProp (OrP x xs) (OrP y ys) = 
 case orderProp x y of
   EQ -> orderProp xs ys
   other -> other
orderProp (OrP _ _) _ = LT
orderProp _ (OrP _ _) = GT 
orderProp (ImpliesP x xs) (ImpliesP y ys) = 
 case orderProp x y of
   EQ -> orderProp xs ys
   other -> other
orderProp (ImpliesP _ _) _ = LT
orderProp _ (ImpliesP _ _) = GT 
orderProp (NotP x) (NotP y) = orderProp x y