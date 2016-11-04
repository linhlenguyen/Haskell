{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Prop where

import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ(Doc,text,char,int,(<>),(<+>),($+$),render)
import Data.List(nub,sort,sortBy)
import Test.QuickCheck hiding(Prop(..))
import Test.HUnit
import Control.Monad(liftM,liftM2)
import qualified MyMap as DM

import Auxfuns
import Boolean 

data Prop a = 
     LetterP a
   | AndP (Prop a) (Prop a)
   | OrP (Prop a) (Prop a)
   | ImpliesP (Prop a) (Prop a)
   | NotP (Prop a)
   | AbsurdP
   | TruthP
 deriving Eq



toProp True = TruthP
toProp False = AbsurdP

-------------------------------------------------------
-- 1 level-deep optimizing operations on Prop that know 
-- about the special properties of TruthP and AbsurdP,
-- and the law of the excluded middle.

conjugate (LetterP x) (NotP (LetterP y)) | x==y = True
conjugate (NotP (LetterP y)) (LetterP x) | x==y = True
conjugate x y = False

same (LetterP x) (LetterP y) = x==y
same x y = False

andP TruthP x = x
andP x TruthP = x
andP AbsurdP x = AbsurdP
andP x AbsurdP = AbsurdP
-- andP x y | same x y = x 
andP x y | x==y = x
andP x y | conjugate x y = AbsurdP
andP x (w@(AndP y z)) | x==y = w
-- andP x (w@(AndP y z)) | same x y = w 
andP x (w@(AndP y z)) | conjugate x y = AbsurdP
andP x y = AndP x y
        
orP TruthP x = TruthP
orP x TruthP = TruthP
orP AbsurdP x = x
orP x AbsurdP = x
-- orP x y | same x y = x 
orP x y | x==y = x
orP x y | conjugate x y = TruthP
-- orP x (w@(OrP y z)) | same x y =w  
orP x (w@(OrP y z)) | x==y = w
orP x (w@(OrP y z)) | conjugate x y = TruthP
orP x y = OrP x y

notP TruthP = AbsurdP
notP AbsurdP = TruthP
notP (NotP (NotP x)) = notP x
notP (NotP x) = x
notP x = NotP x

implyP AbsurdP _ = TruthP
implyP TruthP x = x
implyP x AbsurdP = notP x
implyP x TruthP = TruthP
implyP x y | x==y = TruthP
implyP x y | conjugate x y = y
implyP x y = ImpliesP x y 

equalP TruthP x = x
equalP x TruthP = x
equalP AbsurdP x = notP x
equalP x AbsurdP = notP x
equalP x y | x==y = TruthP
equalP x y | conjugate x y = AbsurdP
equalP x y = (iff x y)

iff x y = (andP x y) `orP` (andP (notP x) (notP y))

xorP TruthP x = notP x
xorP x TruthP = notP x
xorP AbsurdP x = x
xorP x AbsurdP = x
xorP x y | x==y = AbsurdP
xorP x y | conjugate x y = TruthP
xorP x y = notP(iff x y)

------------------------------------  
-- Boolean instances For Prop

instance Simple n => TPD (Prop n) where
  true =  TruthP
  false = AbsurdP
  isTrue TruthP = True
  isTrue x = False
  isFalse AbsurdP = True
  isFalse x = False
  
instance (Ord n,Simple n) => Boolean (Prop n) where
  -- conj x y = (andP x y)  -- andP seems faster than andOpt, even though terms are 1/2 the size
  -- disj x y = (orP x y)   -- same holds for orP (rather than orOpt)
  conj x y = andOpt x y
  disj x y = orOpt x y
  neg = notP
  imply x y = implyP x y
  xor x y = xorP x y
  equal x y = equalP x y
  
instance (Ord n,Monad m,Simple n) => BooleanM m (Prop n) where
  conjM x y = return(conj x y)
  disjM x y = return(disj x y)
  negM x = return(neg x)
  implyM x y = return(disj (neg x) y)
  xorM x y = return(xor x y)
  equalM x y = return(equal x y)

  

----------------------------------------------------------
-- When we operatte on lists of Props, we can get better
-- results if we keep subterms in a cannonical order that
-- moves terms that might cancel close together. Then we
-- can fold the one level operations over lists
-- We want to order lists of Prop such that 
-- AbsurdP < TruthP < (LetterP and Not LetterP with same var) < AndP < OrP < NotP
-- sortBy orderProp [p3,p1 \/ p5,T,p4,~p3,p4,F,~(p4 /\ p5),p2 /\ p7]  -->  
--                  [F,T,p3,~p3,p4,p4,p2 /\ p7,p1 \/ p5,~(p4 /\ p5)]

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
-- orderProp (NotP _) _ = LT  -- these are overlapping, because
-- orderProp _ (NotP _) = GT  -- all other cases have been considered


orL xs = foldr orP AbsurdP (sortBy compare xs)


orOpt x y = foldr orP AbsurdP (sortBy compare (collect (OrP x y)))
  where collect (OrP x y) = collect x ++ collect y
        collect x = [x]


{-
orOpt x y = foldr orP AbsurdP (collect (OrP x y))
  where collect (OrP x y) = merge (collect x) (collect y)
        collect x = [x]
        merge [] xs = xs
        merge ys [] = ys
        merge (zs@(y:ys)) (ws@(x:xs)) =
          case compare y x of
            EQ -> merge zs xs
            LT -> y : merge ys ws
            GT -> x : merge zs xs
-}            

andL xs = foldr andP TruthP (sortBy compare xs)
andOpt x y = foldr andP TruthP (sortBy compare (collect (AndP x y)))
  where collect (AndP x y) = collect x ++ collect y
        collect x = [x]

-- opt all sub terms and then combine with the (op)P functions
opt AbsurdP = AbsurdP
opt TruthP = TruthP
opt (LetterP x) = LetterP x
opt (AndP x y) = andP (opt x) (opt y)
opt (OrP x y) = orP (opt x) (opt y)
opt (ImpliesP x y) = implyP (opt x) (opt y)
opt (NotP x) = notP(opt x)


--------------------------------------------------
-- Semantic optimization uses the meaning of a term
-- (rather than just its syntax) to optimize.
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
-- and then combine them with the (op)B functions

semanticTrans tab AbsurdP = AbsurdP
semanticTrans tab TruthP = TruthP
semanticTrans tab (LetterP x) =
  case DM.lookup x tab of
    Just e -> e
    Nothing -> LetterP x
semanticTrans tab (AndP x y) = andP (semanticTrans (assume y tab) x) (semanticTrans (assume x tab) y)
semanticTrans tab (OrP x y) = orP (semanticTrans (deny y tab) x) (semanticTrans (deny x tab) y)
semanticTrans tab (ImpliesP x y) = implyP (semanticTrans tab x) (semanticTrans (assume x tab) y)
semanticTrans tab (NotP x) = notP (semanticTrans tab x)

optimize x = semanticTrans DM.empty x

------------------------------------------------------
-- evaluating terms under an assignment

-- An assignment is a List
eval:: Eq n => [(n,Bool)] -> Bool -> Prop n -> Bool
eval env missing TruthP = True
eval env missing AbsurdP = False
eval env missing (NotP x) = not(eval env missing x)
eval env missing (AndP x y) = (eval env missing x) && (eval env missing y)
eval env missing (OrP x y) = (eval env missing x) || (eval env missing y)
eval env missing (ImpliesP x y) = imply (eval env missing x) (eval env missing y)
  where imply False _ = True
        imply True x = x
eval env missing (LetterP x) = case lookup x env of
                     Nothing -> missing
                     Just b -> b
                     
-- an assignment is a function
value :: (t -> Bool) -> Prop t -> Bool
value vf TruthP = True
value vf AbsurdP = False
value vf (NotP x) = not (value vf x)
value vf (AndP x y) = (value vf x) && (value vf y)
value vf (OrP x y) = (value vf x) || (value vf y)
value vf (ImpliesP x y) = if (value vf x) then (value vf y) else True
value vf (LetterP x) = vf x

-- generate all possible assignments

assigns [] = []
assigns [n] = [[(n,True)],[(n,False)]]
assigns (x:xs) = map ((x,True):) ys ++ map ((x,False):) ys
  where ys = assigns xs
  
assignments :: Int -> [[Bool]]    
assignments 0 = [[]]

-- assignments 1 = [[True],[False]]
assignments n = map (True :) tail ++ map (False :) tail
  where tail = assignments (n-1)
    
-----------------------------------------------------------------
-- CNF and DNF
-- set up some machinery for uniform notation

alpha :: Prop a -> Maybe (Prop a, Prop a) 
alpha (AndP x y) = return (x,y)
alpha (NotP (OrP x y)) = return (NotP x, NotP y)
alpha (NotP (ImpliesP x y)) = return (x, NotP y)
alpha _ = Nothing
       
beta :: Prop a -> Maybe (Prop a, Prop a)
beta (NotP (AndP x y)) = return (NotP x, NotP y)
beta (OrP x y) = return (x,y)
beta (ImpliesP x y) = return (NotP x, y)
beta _ = Nothing

isLit (NotP (LetterP _)) = True
isLit (LetterP _) = True
isLit AbsurdP = True
isLit TruthP = True
isLit _ = False

cnf :: Eq a => Prop a -> [[Prop a]]
cnf TruthP = []
cnf AbsurdP = [[]]
cnf p = processClause alpha beta [p]

dnf AbsurdP = []
dnf TruthP = [[]]
dnf p = processClause beta alpha [p]

processClause :: Eq a => (Prop a -> Maybe (Prop a, Prop a)) -> 
                   (Prop a -> Maybe (Prop a, Prop a)) ->
                   [Prop a] -> [[Prop a]]
processClause alphy bety [] = [[]]
processClause alphy bety (p : ps) 
  | isLit p = map (add p) $ processClause alphy bety ps
  | otherwise = 
    case p of
      NotP TruthP -> map (AbsurdP:) $ processClause alphy bety ps
      NotP AbsurdP -> map (TruthP:) $ processClause alphy bety ps
      NotP (NotP p') -> processClause alphy bety (p' : ps)
      otherwise -> 
        case bety p of
          Just (x,y) -> processClause alphy bety (x : y : ps)
          Nothing -> 
            case alphy p of
              Just (x,y) -> processClause alphy bety (x : ps) ++ processClause alphy bety (y : ps)
              Nothing -> error "IMPOSSIBLE IN PROCESS CLAUSE"
 where add x xs = if elem x xs then xs else x:xs    

cnf2 :: Prop Int -> [[Prop Int]]
cnf2 TruthP = []
cnf2 AbsurdP = [[]]
cnf2 p = process [p]
process [] = [[]]
process (p:ps) = 
  case p of
   (AbsurdP)             -> map (AbsurdP:) (process ps)
   (TruthP)              -> map (TruthP:) (process ps)
   (LetterP _)           -> map (add p) (process ps)
   (AndP x y)            -> process (x:ps) ++ process (y:ps)
   (OrP x y)             -> process (x : y : ps)
   (ImpliesP x y)        -> process (NotP x : y : ps)  
   (NotP z) -> 
       case z of 
          (AbsurdP)      -> map (TruthP:) (process ps)
          (TruthP)       -> map (AbsurdP:) (process ps)
          (LetterP _)    -> map (add p) (process ps)
          (AndP x y)     -> process (NotP x : NotP y : ps)
          (OrP x y)      -> process (NotP x:ps) ++ process (NotP y:ps)
          (ImpliesP x y) -> process (x:ps) ++ process (NotP y:ps)
          (NotP p2)      -> process (p2:ps)   
          
 where add x xs = if elem x xs then xs else x:xs          


------------------------------------------------------------------------
-- operations on Prop

letters:: (Ord a) => Prop a -> [a]
letters x = vars x []

vars TruthP ans = ans
vars AbsurdP ans = ans
vars (LetterP x) ans = insert x ans
vars (NotP x) ans = vars x ans
vars (AndP x y) ans = vars x (vars y ans)
vars (OrP x y)  ans = vars x (vars y ans)
vars (ImpliesP x y)  ans = vars x (vars y ans)

insert x [] = [x]
insert x (y:ys) | x==y = y:ys
                | x<y  = x:y:ys
                | x>y  = y:(insert x ys)

unionL [] ys = ys
unionL (x:xs) ys = unionL xs (insert x ys)

propSize x = help x (0::Int)
  where help TruthP ans = ans
        help AbsurdP ans = ans
        help (LetterP x) ans = ans
        help (NotP x) ans = help x (1+ ans)
        help (AndP x y) ans = help x (help y (1+ans))
        help (OrP x y)  ans = help x (help y (1+ans))  
        help (ImpliesP x y)  ans = help x (help y (1+ans))
        

instance Functor Prop where fmap = mapProp

mapProp :: (a -> b) -> Prop a -> Prop b
mapProp f (LetterP a) = LetterP (f a)
mapProp f (AndP p q) = AndP (mapProp f p) (mapProp f q)
mapProp f (OrP p q) = OrP (mapProp f p) (mapProp f q)
mapProp f (ImpliesP p q) = ImpliesP (mapProp f p) (mapProp f q)
mapProp f (NotP p) = NotP (mapProp f p) 
mapProp _ AbsurdP = AbsurdP
mapProp _ TruthP = TruthP
                
------------------------------------------------------------------------
-- pretty printing Prop

instance Simple (Prop Int) where
  simple TruthP = "T"
  simple AbsurdP = "F"
  simple (LetterP x) = "p"++show x
  simple (AndP x y) = "And"
  simple (OrP x y) = "Or"
  simple (ImpliesP x y ) = "Implies"
  simple (NotP _) = "Not"
 
precedence (NotP _) = 5
precedence (AndP _ _) = 4
precedence (OrP _ _) = 3
precedence (ImpliesP _ _) = 2
precedence _ = 1

parens n (term@(LetterP _)) = pp term
parens n (term@AbsurdP) = pp term
parens n (term@TruthP) = pp term
parens n (term@(NotP _)) = pp term
-- parens n term = PP.parens (pp term)
parens n term | n /= precedence term = PP.parens (pp term)
              | otherwise = pp term

ands (AndP x y) = ands x ++ ands y
ands x = [x]

ors (OrP x y) = ors x ++ ors y
ors x = [x]

instance Simple a => PP(Prop a) where   
  pp (LetterP a) = text(simple a)
  pp (NotP t) = text "-" <> parens 1 t
  pp (p@(AndP x y)) = -- PP.sep [ parens 4 x, text "/\\", parens 4 y]
    PP.fsep (PP.punctuate (text " /\\") (map (parens 4) (ands p)))
  pp (p@(OrP x y)) = PP.sep [ parens 3 x, text "\\/", parens 3 y]
    -- PP.fsep (PP.punctuate (text " \\/") (map (parens 3) (ors p)))
  pp (ImpliesP x y) = PP.sep [ parens 3 x, text "~>", parens 2 y]
  pp AbsurdP = text "AbsurdP"
  pp TruthP = text "TruthP"

instance Simple a => Show (Prop a) where
  show x = render (pp x)

------------------------------------------------
-- Testing

   
ttt = quickCheck (\ a -> testEq (andL(map orL(cnf2 a))) (andL(map orL(cnf a))))   
   
-----------------------------------------------------------

---------------------------------------------------------------------
-- for testing purposes we write a tautology checker
-- a solution finder, and an equality checker. Each of them
-- generates all possible assignments, and evaluates terms
-- under all of the assignments. Not very efficient, as
-- there are 2^n possible assignments for n variables.
-- A tautology checker

data TautCheck = Tautology | CounterExample [(Int,Bool)] | AlwaysF
 deriving Show

data Taut a
  = Always (Prop a) 
  | CounterEx (Prop a) [Bool]

     
taut:: Prop Int -> TautCheck
taut x = case (assigns (vars x [])) of
           [] -> if (eval [] True x) then Tautology else AlwaysF
           as -> let first [] = Tautology
                     first ((True,e) : xs) = first xs
                     first ((False,e):xs) = CounterExample e
                 in first(map (\ env -> (eval env True x,env)) as)
  
-- testEq x y | vars x [] /= vars y [] = error "Vars are different in tautology checker"
testEq x y = case vs of
            [] -> eval [] True x == eval [] True y
            (_:_) -> and (map (test2terms x y) (assigns vs))
  where vsx = vars x []
        vsy = vars y []
        vs = nub(vsx++vsy)
        test2terms x y env = (eval env True x) == (eval env True y)


-- Solution finders

findSolution x = case allSolutions x of
                   [] -> Nothing
                   (x:xs) -> Just x
        
allSolutions x = first (map (\ env -> (eval env True x,env)) (assigns (vars x [])))
  where first [] = []
        first ((True,e) : xs) = e : first xs
        first ((False,e) : xs) = first xs        

-------------------------------------------------------------
-- Lots of examples

pluseq = cnf(orL [andL [LetterP 3,LetterP 1,LetterP 2]
                 ,andL [LetterP 3,NotP(LetterP 1),NotP(LetterP 2)]
                 ,andL [NotP(LetterP 3),NotP(LetterP 1),LetterP 2]
                 ,andL [NotP(LetterP 3),LetterP 1,NotP(LetterP 2)] ])
minuseq = cnf(orL [andL [NotP(LetterP 3),LetterP 1,LetterP 2]
                 ,andL [NotP(LetterP 3),NotP(LetterP 1),NotP(LetterP 2)]
                 ,andL [(LetterP 3),NotP(LetterP 1),LetterP 2]
                 ,andL [(LetterP 3),LetterP 1,NotP(LetterP 2)] ])
 

test7 = ImpliesP (ImpliesP p (ImpliesP q r)) (ImpliesP (ImpliesP p q) (ImpliesP p r))
  where q = LetterP "q"
        r = LetterP "r"
        p = LetterP "p"


p1,p11,p21 :: Prop Int
[p1,p2,p3,p4,p5,p6,p7,p8,p9,p10] = map LetterP [1..10]  
[p11,p12,p13,p14,p15,p16,p17,p18,p19,p20] = map LetterP [11..20] 
[p21,p22,p23,p24,p25,p26,p27,p28,p29,p30] = map LetterP [21..30] 



b1,b2,b3:: Prop Int
b1 = LetterP 1
b2 = LetterP 2
b3 = LetterP 3

oneZ = orL [ andL [ LetterP 1, NotP(LetterP 2), NotP(LetterP 3) ]
          , andL [ NotP(LetterP 1), LetterP 2, NotP(LetterP 3) ]
          , andL [ NotP(LetterP 1), NotP(LetterP 2), LetterP (3::Int) ]
          ]
twoZ = orL [ andL [ LetterP 1, NotP(LetterP 2), NotP(LetterP 3), NotP(LetterP 4) ]
          , andL [ NotP(LetterP (1::Int)), LetterP 2, NotP(LetterP 3), NotP(LetterP 4) ]
          , andL [ NotP(LetterP 1), NotP(LetterP 2), LetterP 3, NotP(LetterP 4) ]
          , andL [ NotP(LetterP 1), NotP(LetterP 2), NotP(LetterP 3), LetterP 4 ]
          ]          

conjNF x = andL (map orL (cnf x))
disjNF x = orL (map andL (dnf x))

twotwo = (iff twoZ (conjNF twoZ))

go1 = conjNF oneZ
go2 = conjNF twoZ 

-- Run some tests

checkX :: String -> TautCheck -> Assertion
checkX s Tautology = assertBool "Tautology" True
checkX s (CounterExample x) = assertFailure (s++"\n  CounterExample = "++show x)

taut1 = TestCase (assertBool "one = conjNF one" (testEq oneZ go1))
taut2 = TestCase (assertBool "two == conjNF two" (testEq twoZ go2))
taut3 = TestCase (checkX "twotwo is not a tautology" (taut twotwo))
taut4 = TestCase (checkX "equal is negation of xor" 
     (taut (iff (notP(equalP 2 3)) (xorP 2 3))))

booleanTests = TestList [taut1, taut2, taut3, taut4]
testall = runTestTT booleanTests
    
----------------------------------------------------------------

instance Arbitrary a => Arbitrary (Prop a) where
  -- coarbitrary = undefined
  arbitrary = oneof [return AbsurdP, return TruthP
                    , liftM LetterP arbitrary
                    , liftM NotP arbitrary
                    , liftM2 AndP arbitrary arbitrary
                    , liftM2 OrP arbitrary arbitrary
                    ]
temp,temp2,temp3,temp4 :: Prop Int
temp = (AndP (iff (LetterP (3::Int)) (iff (LetterP 1) (LetterP 2))) (iff (LetterP 3) (iff (LetterP 2) (LetterP 1))))

temp2 = (iff (iff (LetterP 3) (iff (LetterP 1) (LetterP 2))) (iff (LetterP 3) (iff (LetterP 2) (LetterP 1))))
temp3 = NotP(AndP (iff (LetterP 1) (LetterP 2)) (NotP (LetterP 3)))

temp4 = NotP(AndP (NotP(iff (LetterP 1) (LetterP 2))) ((LetterP 3)))

{-
*PropOpt> nub $ map (sortBy compare) (cnf temp2)
[[~1,~2,3,~3],[1,2,3,~3],[1,~1,3,~3],[1,~2,3,~3],[~1,2,3,~3],[2,~2,3,~3],[1,~1,2,~2,3,~3],
[1,~1,~2,3],[~1,2,~2,3],[1,~1,2,3],[1,2,~2,3],[1,~1,~2,~3],[1,~1,2,~3],[1,~1,~2,3,~3],[1,~
1,2,3,~3],[1,~1,2,~2,~3],[1,~1,2,~2,3],[1,2,~2,~3],[1,2,~2,3,~3],[~1,2,~2,~3],[~1,2,~2,3,~
3]]
-}


v1 = LetterP 1
v2 = LetterP 2
v3 = LetterP 3
v4 = LetterP 4 
v5 = LetterP 5
v6 = LetterP 6
v7 = LetterP 7
v8 = LetterP 8
v9 = LetterP 9 
v0 = LetterP 0

propStat :: Prop n -> (Int,Int,Int)  
propStat TruthP = (0,1,0)
propStat AbsurdP = (0,1,0)
propStat (AndP x y) = (max d1 d2 + 1,s1+s2+1,l1+l2)
  where (d1,s1,l1) = propStat x
        (d2,s2,l2) = propStat y
propStat (OrP x y) = (max d1 d2 + 1,s1+s2+1,l1+l2)
  where (d1,s1,l1) = propStat x
        (d2,s2,l2) = propStat y 
propStat (ImpliesP x y) = (max d1 d2 + 1,s1+s2+1,l1+l2)
  where (d1,s1,l1) = propStat x
        (d2,s2,l2) = propStat y         
propStat (NotP x) = (d+1,s+1,l) where (d,s,l) = propStat x
propStat (LetterP x) = (0,1,1) 



-----------------------------------------------
-- This is class hackery, use at your own risk
-- used for manually inputing (Prop Int) from the keyboard

--- Everyone's using Nathan Collins' syntactic sugar now.
infixr 3 /\
infixr 2 \/
infixr 1 ~>

(/\),(\/),(~>) :: Prop Int -> Prop Int -> Prop Int
p /\ q = AndP p q
p \/ q = OrP p q
p ~> q = ImpliesP p q

instance Num (Prop Int) where
  fromInteger n = LetterP (fromInteger n)
  x + y = OrP x y
  x * y = AndP x y
  abs x = undefined
  signum x = undefined
  negate x = NotP x