module Tableau2 where

import SimpleProp
import Blocks
import qualified Test.QuickCheck as QC
import Test.QuickCheck hiding (Prop)
import Control.Monad(liftM,liftM2)


data Tree n 
  = Direct (Bool,Prop n) (Tree n)
  | Branch (Tree n) (Tree n)
  | Leaf 

------------------------------------------------
-- code for printing and drawing trees

instance PPLetter n => Show (Tree n) where
  show Leaf = ""
  show (Branch x y) = "["++show x ++ "/\\"++ show y++"]"
  show (Direct (True,p) t) = "{T "++show p++" "++show t++"}"
  show (Direct (False,p) t) = "{F "++show p++" "++show t++"}"  

lblock (True,p) = beside True (oneBlock "T ")(oneBlock (show p))
lblock (False,p) = beside True (oneBlock "F ")(oneBlock (show p))

tblock Leaf = oneBlock ""
tblock (Direct x t) = above (center (lblock x) w) bottom
   where bottom = (tblock t)
         w = width bottom
tblock (Branch x y) = (sep True 2 bs)
    where bs = map (box 0 . tblock) [x,y]
          w = sum (map width bs)

-----------------------------------------------------
-- the two examples in the Smullyan text

item = (0 + (1 * 2)) ~> ((0 + 1)*(0+2))
tree :: Tree Int
tree = mk2 False item
blk = tblock tree

item2 = (p ~> (q ~> r)) ~> ((p ~> q) ~> (p ~> r))
  where p = LetterP "p"; q = LetterP "q"; r = LetterP "r"
tree2 :: Tree String
tree2 = mk2 False item2
blk2 = tblock tree2


bad = (0 + (1 * 2)) ~> ((0 + 2)*(1+2))

-- This code mimics the by hand construction of 
-- Signed tableau, by direct inspection the Sign
-- and structure of the term. This is somewhat AdHoc

mk2 :: Bool -> Prop n -> Tree n
mk2 b (NotP x) = Direct (b,NotP x) (mk2 (not b) x)
mk2 True (AndP x y) = Direct (True,AndP x y) (push (mk2 True y) (mk2 True x))
mk2 False (AndP x y) = Direct (False,AndP x y) (Branch (mk2 False x) (mk2 False y))
mk2 True (OrP x y) = Direct (True,OrP x y) (Branch (mk2 True x) (mk2 True y))
mk2 False (OrP x y) = Direct (False,OrP x y) (push (mk2 False y) (mk2 False x))
mk2 True (ImpliesP x y) = Direct (True,ImpliesP x y) (Branch (mk2 False x) (mk2 True y))
mk2 False (ImpliesP x y) = 
  Direct (False,ImpliesP x y) (push (mk2 False y) (mk2 True x))
mk2 bool prop = Direct (bool,prop) Leaf


push:: Tree n -> Tree n -> Tree n
push Leaf t = t
push t Leaf = t
push (Direct s m) (Direct t n) = Direct t (Direct s (push m n))
push t (Direct pair m) = Direct pair (push t m)
push (Direct s m) (Branch x y) = Direct s (Branch (push m x) (push m y))
push t (Branch  x y) = Branch (push t x) (push t y)

----------------------------------------------
-- Is a tree closed? I.e. it is not satisfiable

extendTruths (True,p) truths = p:truths
extendTruths (False,p) truths = (NotP p):truths

closed:: Eq n => Tree n -> [Prop n] -> Bool
closed Leaf truths = False
closed (Direct (pair@(True,p)) tree) truths =
   elem (NotP p) truths || closed tree (extendTruths pair truths)
closed (Direct (pair@(False,p)) tree) truths =
   elem p truths || closed tree (extendTruths pair truths)
closed (Branch x y) truths = 
   closed x truths && closed y truths
                     
----------------------------------------------------
-- Now build a tree using Alpha and Beta rules
-- This we we have far fewer cases. This is the
-- same function we used for implementing CNF
                  
data Discrim a = Alpha a a | Beta a a | Lit a
 deriving Show

discrim :: Prop a -> Discrim (Prop a)
discrim TruthP = Lit TruthP
discrim AbsurdP = Lit AbsurdP
discrim (LetterP s) = Lit (LetterP s)
discrim (AndP x y) = Alpha x y
discrim (OrP x y) = Beta x y
discrim (ImpliesP x y) = Beta (NotP x) y
discrim (NotP (OrP x y)) = Alpha (NotP x)  (NotP y)
discrim (NotP (ImpliesP x y)) = Alpha x (NotP y)
discrim (NotP (AndP x y)) = Beta (NotP x) (NotP y)
discrim (NotP (NotP x)) = discrim x
discrim (NotP TruthP) = Lit AbsurdP
discrim (NotP AbsurdP) = Lit TruthP
discrim (NotP (LetterP s)) = Lit (NotP (LetterP s))

---------------------------------------------------
-- Build a tableau according to page 24 of Smullyan

-- make 1 and 2 elements trees 
single p = Direct (True,p) Leaf
double p q = Direct (True,p) (single q)
                    
-- invariant: elements of the list are in 
-- the tree but not yet "used"                    
tabTree [] tree = tree
tabTree (x:xs) tree =
  case discrim x of
    Lit p -> tabTree xs tree
    Alpha a b -> tabTree (a:b:xs) (extendTree (double a b) tree)
    Beta a b -> extendTree 
                   (Branch (tabTree (a:xs) (single a))
                           (tabTree (b:xs) (single b)))
                   tree

extendTree p Leaf = p
extendTree p (Direct q t) = Direct q (extendTree p t)
extendTree p (Branch x y) = Branch (extendTree p x) (extendTree p y)

solveT p = tblock (tabTree [NotP p] (single (NotP p)))

----------------------------------------------------------------
-- An optimization is to keep the elements in the "not used"
-- list ordered in a way that keeps Alpha terms first. 
-- This makes the tree deep, rather than wide.


-- Try and choose Alpha rules before Beta Rules
cmp x y = 
  case (discrim x,discrim y) of
    (Alpha _ _, _) -> GT
    (Lit _, _) -> GT
    (Beta _ _,_) -> LT
    
insert x [] = [x]
insert x (y:ys) = 
  case cmp x y of
    GT -> x : y : ys
    other -> y : insert x ys
    
tabTree2 [] tree = tree
tabTree2 (x:xs) tree =
  case discrim x of
    Lit p -> tabTree2 xs tree
    Alpha a b -> tabTree2 (insert a (insert b xs)) (extendTree (double a b) tree)
    Beta a b -> extendTree 
                   (Branch (tabTree2 (insert a xs) (single a))
                           (tabTree2 (insert b xs) (single b)))
                   tree
                       
solveT2 p = tblock (tabTree2 [NotP p] (single (NotP p)))    
    
---------------------------------------------------
-- Now we will try a use the Tableau method without
-- actually constructing the Tree. Instead we will
-- construct a list of paths in the tree.
-- The simplest version, no attempt at optimization

tab1 [] paths = paths
tab1 (x:xs) paths =
  case discrim x of
    Lit p -> tab1 xs (map (p:) paths)
    Alpha a b -> tab1 (a : b : xs) (map (\ path -> a:b:path) paths)
   -- Beta a b -> tab1 (a : b: xs) [ p:ps | p <- [a,b],ps <- paths]
   -- I first thought the above would work but it adds extra bogus paths
    Beta a b -> tab1 (a:xs) (map (a:) paths) ++
                tab1 (b:xs) (map (b:) paths)
    
solve1 p = tab1 [NotP p] [[]]    

----------------------------------------------------------------    
-- Use the optimization of using Alpha nodes first.
-- reuse the "insert" function from "tabTree2" above.
    
tab2 [] paths = paths
tab2 (x:xs) paths =
  case discrim x of
    Lit p -> tab2 xs (map (p:) paths)
    Alpha a b -> tab2 (insert a (insert b xs)) (map (\ path -> a:b:path) paths)
    Beta a b -> tab2 (insert a xs) (map (a:) paths) ++
                tab2 (insert b xs) (map (b:) paths)
    
solve2 p = tab2 [NotP p] [[]]       

----------------------------------------------------
-- Simplify paths, Keep only literals, and keep
-- only one copy

isLit (LetterP x) = True
isLit (NotP (LetterP x)) = True
isLit TruthP = True
isLit AbsurdP = True
isLit x = False

cons3 x xs | not (isLit x) = xs
cons3 x xs | elem x xs = xs
cons3 x xs = x : xs
    
insert3 x xs = x :xs
    
tab3 [] paths = paths
tab3 (x:xs) paths =
  case discrim x of
    Lit p -> tab3 xs (map (cons3 p) paths)
    Alpha a b -> tab3 (insert3 a (insert3 b xs)) (map (cons3 a . cons3 b) paths)
    Beta a b -> tab3 (insert3 a xs) (map (cons3 a) paths) ++
                tab3 (insert3 b xs) (map (cons3 b) paths)    
                
solve3 p = tab3 [NotP p] [[]]   

----------------------------------------------------
-- Keep two lists of paths, active, and closed
-- a closed path has a contradiction.

conjugatePair [] = False
conjugatePair [x] = False
conjugatePair (LetterP x : xs) = elem (NotP (LetterP x)) xs
conjugatePair (NotP x : xs) = elem x xs
conjugatePair (x:xs) = conjugatePair xs

split ([],closed) = ([],closed)
split (x:xs,closed) | conjugatePair x = split (xs,x:closed)
split (x:xs,closed) = (x:ys,zs) where (ys,zs) = split (xs,closed)

tab4 :: Eq n =>  [Prop n] -> ([[Prop n]],[[Prop n]]) -> ([[Prop n]],[[Prop n]])
tab4 [] (paths,closed) = (paths,closed)
tab4 (x:xs) (paths,closed) =
  case discrim x of
    Lit p -> tab4 xs (split(map (cons3 p) paths,closed))
    Alpha a b -> tab4 (insert3 a (insert3 b xs)) 
                      (split(map (cons3 a . cons3 b) paths,closed))
    Beta a b -> 
       merge (tab4 (insert3 a xs) (split (map (cons3 a) paths,[])))
             (tab4 (insert3 b xs) (split (map (cons3 b) paths,[])))
             closed

merge (xs,ys) (ws,zs) closed = (xs++ws,ys++zs++closed)      
    
    
solve4 p = tab4 [NotP p] ([[]],[])

----------------------------------------------------
--  Combine the two-lists and putting Alpha terms
-- earlier in the work list. Note that we may cutoff
-- all paths before we have used the Beta terms that
-- do unnecessary splitting

 
tab5 [] (paths,closed) = (paths,closed)
tab5 (x:xs) (paths,closed) =
  case discrim x of
    Lit p -> tab5 xs (split(map (cons3 p) paths,closed))
    Alpha a b -> tab5 (insert a (insert b xs)) 
                      (split(map (cons3 a . cons3 b) paths,closed))
    Beta a b -> 
       merge (tab4 (insert a xs) (split (map (cons3 a) paths,[])))
             (tab4 (insert b xs) (split (map (cons3 b) paths,[])))
             closed
    
solve5 p = tab5 [NotP p] ([[]],[])

--------------------------------------------------
-- How do we prove a property P about Trees?
-- An induction proof would have three parts.
-- 1) Show P(Leaf)
-- 2) Assume P(x), then show P(Direct (t,p) x)
-- 3) Assume P(x) and P(y), then show P(Branch x y)

-- How do we prove a property P about (Prop n)?
-- An induction proof would have 7 parts
-- 1) Show P(TruthP)
-- 2) Show P (AbsurdP)
-- 3) Show P (LetterP x)
-- 4) Assume P(x) and then show P(NotP x)
-- 5) Assume P(x) and P(y) and then show P(AndP x y)
-- 6) Assume P(x) and P(y) and then show P(OrP x y)
-- 7) Assume P(x) and P(y) and then show P(ImpliesP x y)

-- How do we prove a property about Discrim?
-- Three cases
-- 1) Show P(Let x)
-- 2) Show P(Alpha x y)
-- 3) Show P(Beta x y)

alpha1 (Alpha x y) = x
alpha2 (Alpha x y) = y
beta1 (Beta x y) = x
beta2 (Beta x y) = y

conjugate (NotP x) = x
conjugate x = NotP x

-----------------------------------------------
-- Rules of discrimnation

prop1 :: Prop Int -> Property
prop1 p = taut p ==>
  (case discrim p of
    Lit x -> taut x
    Alpha x y -> taut x && taut y
    Beta x y -> taut x || taut y)
    
prop1a f p = taut p ==>
  (case discrim p of
    Lit x -> val f x
    Alpha x y -> val f x && val f y
    Beta x y -> val f x || val f y)    
    
alpha x = 
  case discrim x of
    (Alpha _ _) ->  True
    (Beta _ _) -> False
    (Lit _) -> False

beta x = 
  case discrim x of
    (Alpha _ _) ->  False
    (Beta _ _) -> True
    (Lit _) -> False
    
prop2,prop3:: Prop Int -> Property
prop2 x = alpha x ==>  beta(conjugate x)
prop3 x = beta x ==> alpha(conjugate x)

prop4 zs x = alpha x ==>
             val zs (alpha1 (discrim x)) &&
             val zs (alpha2 (discrim x)) ==> 
             val zs x

-- Not every set is downward and upward closed!!
-- these are definitions not properties
downwardClosed set =
  and [ case discrim x of
         Alpha a b -> elem a set && elem b set
         Beta a b -> elem a set || elem b set
         Lit _ -> True
      | x <- set ]
      
upwardClosed set =
 and  [ case (discrim x, discrim y) of
         (Alpha _ _, Alpha _ _) -> elem (AndP x y) set
         (Beta _ _,Beta _ _) -> elem (OrP x y) set
         other -> True
      | x <- set, y <- set ]
  





---------------------------------------------------

instance Arbitrary a => Arbitrary (Prop a) where
  -- coarbitrary = undefined
  arbitrary = oneof [return AbsurdP, return TruthP
                    , liftM LetterP arbitrary
                    , liftM NotP arbitrary
                    , liftM2 AndP arbitrary arbitrary
                    , liftM2 OrP arbitrary arbitrary
                    ]
   
   
addvars:: Ord a => Prop a -> [a] -> [a] 
addvars TruthP ans = ans
addvars AbsurdP ans = ans
addvars (LetterP x) ans = insertX x ans
addvars (NotP x) ans = addvars x ans
addvars (AndP x y) ans = addvars x (addvars y ans)
addvars (OrP x y)  ans = addvars x (addvars y ans)
addvars (ImpliesP x y)  ans = addvars x (addvars y ans)

insertX x [] = [x]
insertX x (y:ys) | x==y = y:ys
                 | x<y  = x:y:ys
                 | x>y  = y:(insertX x ys)

vars:: Ord a => Prop a -> [a] 
vars x = addvars x []

assigns:: [Int] -> [[(Int,Bool)]]
assigns [] = []
assigns [n] = [[(n,True)],[(n,False)]]
assigns (x:xs) = map ((x,True):) ys ++ map ((x,False):) ys
  where ys = assigns xs
  
taut:: Prop Int -> Bool
taut prop = and [ value (lift x) prop | x <- assigns (vars prop)]
  where lift pairs x = case lookup x pairs of
                         Just b -> b
                         other -> False
                         
val xs prop = value (lift xs) prop   
   where lift pairs x = case lookup x pairs of
                         Just b -> b
                         other -> False
   