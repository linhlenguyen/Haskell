{-# Language FlexibleInstances #-}

module Cnf where

import Auxfuns
import Prop hiding (alpha,conjugate,two,one,beta)
import qualified MyMap as DM
import qualified Data.Set as Set
import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ(Doc,text,char,int,(<>),(<+>),($+$),render)
import Data.Ratio
import Data.List(sortBy,nub)
import Minisat
import Data.IORef(newIORef,readIORef,writeIORef,IORef)
import System.IO.Unsafe(unsafePerformIO)

data Lit = Pos Int | Neg Int deriving Eq

data Alpha = A (Set.Set Lit) (Set.Set Beta) deriving Eq

data Beta  = B (Set.Set Lit) (Set.Set Alpha) deriving Eq

data PropF alpha beta
  = TopF
  | BotF
  | AndF alpha
  | OrF beta
  | LF Lit
 deriving (Eq,Ord)

type Prop2 = PropF Alpha Beta

--------------------------------------------------
conjugate (Pos x) (Neg y) | x==y = True
conjugate (Neg x) (Pos y) | x==y = True
conjugate _ _ = False

conjElem x set = Set.member (flop x) set

fliP TopF = BotF
fliP BotF = TopF
fliP (LF x) = LF(flop x)

flop (Pos x) = (Neg x)
flop (Neg x) = (Pos x)

-- Operations on sets
emptySet = Set.empty
add x set = Set.insert x set
one x = add x emptySet
two x y = add x (add y emptySet)
delete x set = Set.delete x set

-- Generic walks over terms

foldProp :: (Int -> Lit -> b -> b) -> b -> Prop2 -> b
foldProp acc base x =
  case x of
    LF x -> acc 1 x base
    AndF a -> foldA acc base a
    OrF b -> foldB acc base b
    other -> base

foldA :: (Int -> Lit -> b -> b) -> b -> Alpha -> b    
foldA acc base (A is xs) 
   = Set.foldr (\ b ans -> foldB acc ans b) 
               (Set.foldr (acc (Set.size is)) base is) xs    

foldB :: (Int -> Lit -> b -> b) -> b -> Beta -> b
foldB acc base (B is xs) 
   = Set.foldr (\ b ans -> foldA acc ans b) 
               (Set.foldr (acc (Set.size is)) base is) xs    
               
------------------------------------------------
-- Ord instances

instance Ord Beta  where
  compare (B is xs) (B js ys) = 
      compare (Set.size is,is,Set.size xs,xs) 
              (Set.size js,js,Set.size ys,ys)

instance Ord Alpha where
  compare (A is xs) (A js ys) = 
      compare (Set.size is,is,Set.size xs,xs) 
              (Set.size js,js,Set.size ys,ys)

instance Ord Lit where
  compare (Pos x) (Neg y) | x==y = LT
  compare (Neg x) (Pos y) | x==y = GT
  compare (Pos x) (Neg y) = compare x y
  compare (Neg y) (Pos x) = compare y x
  compare (Pos x) (Pos y) = compare x y
  compare (Neg x) (Neg y) = compare x y

--------------------------------------------------------
-- translate a Prop Int into a Prop2

trans:: Prop Int -> Prop2
trans x =
  case discrim x of
    TopF -> TopF
    BotF -> BotF
    LF x -> (LF x)
    AndF a -> (alpha (AndF a) TopF)
    OrF b -> (beta (OrF b) BotF)
   
discrim :: Prop Int -> PropF Alpha Beta
discrim TruthP = TopF
discrim AbsurdP = BotF
discrim (AndP x y) = alpha (discrim x) (discrim y)
discrim (OrP x y) = beta (discrim x) (discrim y)
discrim (ImpliesP x y) = beta (discrim (notP x)) (discrim y)
discrim (LetterP s) = LF (Pos s)
discrim (NotP (LetterP s)) = LF (Neg s)
discrim (NotP TruthP) = BotF
discrim (NotP AbsurdP) = TopF
discrim (NotP (NotP x)) = discrim x
discrim (NotP (AndP x y)) = beta (discrim (notP x)) (discrim (notP y))
discrim (NotP (OrP x y)) = alpha (discrim (notP x)) (discrim (notP y))
discrim (NotP (ImpliesP x y)) = alpha (discrim x) (discrim (notP y))

-- Combine two PropF structures in an Or context
beta :: PropF Alpha Beta -> PropF Alpha Beta -> PropF Alpha Beta
beta TopF x = TopF
beta x TopF = TopF
beta BotF (OrF (B is xs)) 
  | (Set.null is) && (Set.size xs == 1) 
  = (AndF(head(Set.toList xs)))
beta (OrF (B is xs)) BotF 
  | (Set.null is) && (Set.size xs == 1) 
  = (AndF(head(Set.toList xs)))                          
beta BotF x = x
beta x BotF = x
beta (LF x) (LF y) 
  | x==y = LF x
  | conjugate x y = TopF
  | otherwise = OrF (B(two x y) emptySet)
beta (LF x) (OrF (B set xs)) | conjElem x set = TopF
beta (LF x) (OrF (B set xs)) = OrF (B (add x set) xs)
beta (OrF (B set xs)) (LF x) | conjElem x set = TopF
beta (OrF (B set xs)) (LF x) = OrF (B (add x set) xs)
beta (LF x) (AndF z) = OrF (B (one x) (one z))
beta (AndF z) (LF x) = OrF (B (one x) (one z))
beta (OrF (B s1 xs)) (OrF (B s2 ys)) = 
  case mergeSetLit s1 s2 of
    Nothing -> TopF
    Just s3 -> OrF (B s3 (Set.union xs ys)) 
beta (OrF (B s xs)) (AndF z) = OrF (B s (add z xs))
beta (AndF z) (OrF (B s xs)) = OrF (B s (add z xs))
beta (AndF z) (AndF w) = OrF (B emptySet (two z w))

-- Combine two PropF structures in an And context
alpha :: PropF Alpha Beta -> PropF Alpha Beta -> PropF Alpha Beta
alpha TopF (AndF (A is xs)) 
  | (Set.null is) && (Set.size xs == 1) 
  = (OrF(head(Set.toList xs)))
alpha (AndF (A is xs)) TopF 
  | (Set.null is) && (Set.size xs == 1) 
  = (OrF(head(Set.toList xs)))                            
alpha TopF x = x
alpha x TopF = x
alpha BotF x = BotF
alpha x BotF = BotF
alpha (LF x) (LF y) 
  | x==y = LF x
  | conjugate x y = BotF
  | otherwise = AndF (A(two x y) emptySet)
alpha (LF x) (AndF(A set xs)) | conjElem x set = BotF
alpha (LF x) (AndF(A set xs)) = AndF (A (add x set) xs)
alpha (AndF(A set xs)) (LF x) | conjElem x set = BotF
alpha (AndF(A set xs)) (LF x) = AndF (A (add x set) xs)
alpha (LF x) (OrF z) = AndF (A (one x) (one z))
alpha (OrF z) (LF x) = AndF (A (one x) (one z))
alpha (AndF (A s1 xs)) (AndF (A s2 ys)) = 
  case mergeSetLit s1 s2 of
    Nothing -> BotF
    Just s3 -> AndF (A s3 (Set.union xs ys)) 
alpha (AndF (A s xs)) (OrF z) = AndF (A s (add z xs))
alpha (OrF z) (AndF (A s xs)) = AndF (A s (add z xs))
alpha (OrF z) (OrF w) = AndF (A emptySet (two z w))
       
-- Can two sets of literals be added to make a consistent set?

mergeSetLit env set = Set.foldl acc (Just env) set
  where acc Nothing _ = Nothing
        acc (Just env) x | conjElem x env = Nothing
        acc (Just env) x = Just(add x env)
        
--------------------------------------------------------
-- To apply the distributive law of Or over And
-- turn a Beta term like Or[1,4 , And[3,6], And[2,5]] 
-- into a list of list. Each sublist is implicitly "And"ed
-- together.  [[1],[4],[3,6],[2,5]]

flatB :: Beta -> [[PropF Alpha Beta]]
flatB (B is xs) = map g (Set.toList is) ++ map f (Set.toList xs)
  where f (A is xs) = (map (\ x -> LF x) (Set.toList is)) ++ map OrF (Set.toList xs)
        g x = [LF x]
        
-- Then, from each sublist draw one element
-- oneEach [[1],[4],[3,6],[2,5]] -->
--    [[1,4,3,2],[1,4,3,5],[1,4,6,2],[1,4,6,5]]

oneEach :: [[a]] -> [[a]]
oneEach [] = [[]]
oneEach (x:xs)  = [ y : ys | y <- x, ys <- oneEach xs]

-- By implicitly "Or"ing these we have distributed Or over And
-- Thus turning this    Or[1,4 , And[3,6], And[2,5]]    into
-- And[ Or[1,4,3,2],Or[1,4,3,5],Or[1,4,6,2],Or[1,4,6,5]]
        
orOverAnd limit (OrF b) = 
     if tooBig limit 1 zs
        then applyUnitFacts(OrF b)
        else let ws = oneEach zs
                 term = foldr alpha TopF (map (foldr beta BotF) ws)
             in applyUnitFacts term
  where zs = flatB b               
orOverAnd limit x = x

-- But we don't want to do this if the number of sublists
-- is too large. This can get really big fast!

tooBig limit n xs | n > limit = True
tooBig limit n (x:xs) = tooBig limit (n*length x) xs
tooBig limit n [] = n > limit

distr limit x = (orOverAnd limit x)

--------------------------------------------------------------------
-- To turn something into Conjunctive Normal Form we walk over
-- the term and turn each nested And into a new variable.
--  Or[3,-6,And[2,4]]  --> Or[3,-6,99]  and add  (99 => And[2,4])
-- (which is [[-99,2],[-99,4]] in cnf), to the list of 
-- clauses generated. See the function "introNewVar" below.
-- To get started we apply unit facts, and then distribute orOverAnd
-- only for a top-level Or with a limit on how much we will expand.

cnF :: Int -> Prop2 -> (Int,[[Lit]])
cnF next x = case orOverAnd 8 (applyUnitFacts x) of
  TopF -> (next,[])
  BotF -> (next,[[]])
  (LF x) -> (next,[[x]])
  (OrF (B js ys)) ->  (n3,[Set.toList js ++ reverse lits]++deep)
     where (deep,lits,n3) = introNewVar next ys    
  (AndF (A is xs)) -> (n4,toUnit is ++ shallow ++ deep)
    where (n4,shallow,deep) = Set.foldl acc (next,[],[]) xs
          acc (n2,ws2,zs2) (B js ys) = (n3,(Set.toList js ++ reverse lits) : ws2,pairs++zs2)
             where (pairs,lits,n3) = introNewVar n2 ys
          toUnit is = map (:[]) (Set.toList is)
                       
introNewVar n set = Set.foldl acc ([],[],n) set
  where acc (subCls,lits,n) term = 
            let (n2,clauses) = cnF (n+1) (AndF term)
            in ((map ((Neg n):) clauses)++subCls,(Pos n : lits),n2)

prop2 = unsafePerformIO(newIORef (TopF::Prop2))


writeCnf :: [Char] -> Int -> Prop Int -> IO Int
writeCnf file next prop =
  do { let propTwo = (trans prop)
           (n,lists) = cnF next propTwo
     ; writeIORef prop2 propTwo
     ; putStrLn("\nProp2 is\n"++show propTwo)
     ; let f (Pos x) = LetterP x
           f (Neg x) = NotP(LetterP x)
     ; putClauses file (map (map f) lists)
     ; return(n)
     }

toCnf:: Prop Int -> IO(Int,[[Prop Int]])
toCnf prop =
  do { let propTwo = (trans prop)
           next = (maxVar propTwo + 2)
     ; putStrLn("CNF introduces new vars starting at "++show next)
     ; let (n,lists) = cnF next propTwo
     ; writeIORef prop2 propTwo
     ; putStrLn ("Prop2 is\n"++show propTwo)
     ; let f (Pos x) = LetterP x
           f (Neg x) = NotP(LetterP x)
     ; return(n,map (map f) lists)
     }

-------------------------------------------------------------------
-- Partial evaluation. Partial because some
-- variables may not be in the environment.

evalLit:: (Set.Set Lit) -> Lit -> PropF x y
evalLit env x = if Set.member x env 
                   then TopF
                   else if Set.member (flop x) env
                           then BotF
                           else (LF x)
        
evalSetA env set = Set.foldl acc TopF set
  where acc x lit = alpha (evalLit env lit) x
  
evalSetB env set = Set.foldl acc BotF set
  where acc x lit = beta (evalLit env lit) x

-- In And and Or terms with literals we can exploit the literals.
-- We call this using unit facts
-- For example in:
-- And[3,-6 , Or[2,6,7],Or[2,4]]  
--   we may assume 3 and -6 to simplify  Or[2,6,7] and   Or[2,4]] 
--   getting And[3,-6, Or[2,F,7],Or[2,4]] --> And[3,-6, Or[2,7],Or[2,4]]
-- and in:
-- Or[3,-6 , And[2,6,7],And[2,4]] 
--   we may assume their negations -3 and 6 to simplify And[2,6,7] and And[2,4]
--   getting Or[3,-6 , And[2,F,7],And[2,4]] --> Or[3,-6,And[2,4]]

evalPropA:: Set.Set Lit -> Alpha -> PropF Alpha Beta
evalPropA env (A is xs) = 
  case mergeSetLit env is of
     Nothing -> BotF
     Just env2 -> Set.foldl (\ x y -> alpha (evalPropB env2 y) x) 
                            (evalSetA env is) -- use original env here
                            xs
              
evalPropB:: Set.Set Lit -> Beta -> PropF Alpha Beta              
evalPropB env (B is xs) = 
  case mergeSetLit env (Set.map flop is) of
    Nothing -> TopF
    Just env2 -> Set.foldl (\ x y -> beta (evalPropA env2 y) x) 
                           (evalSetB env is) -- use original env here
                           xs

evalF env (LF x) = evalLit env x
evalF env TopF = TopF
evalF env BotF = BotF
evalF env (AndF a) = evalPropA env a
evalF env (OrF b) = evalPropB env b

applyUnitFacts x = evalF emptySet x

simp x = applyUnitFacts x

-- Apply a substitution to a term.
evalProp2 env x = (evalF env x)

----------------------------------------------------------------
-- to solve a term (i.e. find a satisfying assignment)
-- we propogate unit clauses (Top-level Ands only)
-- When this is no longer possible we choose a good variable
-- and they try both possibilities. Lots of ways to improve this.

solve:: Int -> Set.Set Lit -> Prop2 -> IO (Set.Set Lit, Prop2)                                 
solve _ env (term@(TopF)) =
  do { putStrLn("\nSolution is "++show env)
     ; return(env,term) }
solve 0 env x = return(env,x)      
solve n env (term@((AndF (A is xs)))) | Set.null is =
  maybeM (chooseLit term env)
    (return(env,term))
    (\ lit -> do { putStrLn("\n--------------\nChoosing literal "++show lit)
                 ; (env2,f) <- solve (n-1) env ((AndF (A (one lit) xs)))
                 ; case f of
                     BotF -> do { putStrLn("\nBackTracking "++show (flop lit))
                                ; solve (n-1) env ((AndF (A (one (flop lit)) xs))) }
                     other -> return(env2,f)})
-- This is the unit propogation case
solve n env (term@((AndF (A is xs)))) =
  maybeM (mergeSetLit env is)
    (return (env,BotF))
    (\ env2 -> do { let new = evalProp2 env2 ((AndF (A emptySet xs)))
                  -- ; putStrLn("\n----------------------------\n"++show new)
                  ; solve n env2 new })
solve n env x = return(env,x)                  


chooseLit x env = 
  case prop2Vars x of
    [] -> Nothing
    ((v,score):vs) -> if goodChoice v env
                         then Just v
                         else Nothing

goodChoice lit env =
  if conjElem lit env
     then False
     else if Set.member lit env then False else True

-------------------------------------------------------
-- compute the literals (and their context) from a term.

maxVar x = foldProp acc 0 x
  where acc i (Pos n) ans = max n ans
        acc i (Neg n) ans = max n ans
    
accUse i x ans = DM.insertWith (+) (flop x) (1 % i) ans

prop2Vars x = sortBy test (DM.toList(foldProp accUse DM.empty x))
  where test (x,y) (a,b) = compare (b,x) (y,a)

-------------------------------------------------
-- Pretty Printing

ppLit (Pos x) = text(show x)
ppLit (Neg x) = text("-"++show x)

ppSet s zs | Set.null s && null zs = PP.empty
ppSet s zs = (PP.fsep(PP.punctuate (text ",") (map ppLit (Set.toList s)))) <> 
             (if (null zs) then PP.empty else text ",")

docsA (A s zs) = (map ppLit (Set.toList s),map ppB (Set.toList zs))
docsB (B s zs) = (map ppLit (Set.toList s),map ppA (Set.toList zs))

sepBy comma xs = PP.fcat(PP.punctuate (text comma) xs)

ppA a = 
  case docsA a of
    ([],xs) -> text "And" <> PP.brackets (sepBy "," xs)
    (is,[]) -> text "And" <> PP.brackets (sepBy "," is)
    (is,xs) -> text "And" <> PP.brackets (PP.sep[sepBy "," is,text ",",sepBy "," xs])
  
ppB b = 
  case docsB b of
    ([],xs) -> text "Or" <> PP.brackets (sepBy "," xs)
    (is,[]) -> text "Or" <> PP.brackets (sepBy "," is)
    (is,xs) -> text "Or" <> PP.brackets (PP.sep[sepBy "," is,text ",",sepBy "," xs])
  
ppPropF (TopF) = text "T"
ppPropF (BotF) = text "F"
ppPropF ((LF x)) = ppLit x
ppPropF ((AndF x)) = ppA x
ppPropF ((OrF x)) = ppB x

instance Show Lit  where
  show x = render(ppLit x)  

instance Show (PropF Alpha Beta) where
  show x = render(ppPropF x)

instance Show (Beta) where
  show x = render(ppB x)

instance Show (Alpha) where
  show x = render(ppA x)
  
-------------------------------------------------------------------------
  
tt1 = (9 + 10 + (1 * 2 * 3) + (5 * 4) + (7 * (8 + (6 * 12)))) :: Prop Int

tt7,tt8 :: Prop Int       
tt7 = ((1 /\ 5)/\ ((-1) /\ 4))
tt8 = ((1 \/ 5)\/ ((-1) \/ 4))


test33 = testp (trans temp)

testq ((AndF b)) = (AndF b)
testq ((OrF b)) = (foldr alpha TopF (map (foldr beta BotF) ws))
  where zs = flatB b
        ws = oneEach zs

testp ((AndF n)) = (evalF emptySet (AndF n))
testp ((OrF b)) = (evalF emptySet (foldr alpha TopF (map (foldr beta BotF) ws)))
  where zs = flatB b
        ws = oneEach zs        

z3 = trans temp



