{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, 
    DeriveDataTypeable, FlexibleInstances,
    RankNTypes, GADTs, ScopedTypeVariables  #-}

module FiniteSet where

import Dimension
import Prop
import Boolean
import Bdd hiding (toLit,neg)
import Auxfuns(PP(..),plistf,foldlM,ifM)
import Literal
                    
import qualified MyMap as DM 
import MyMap hiding (map,filter,union,difference,foldr)
import Data.List(nub,find,elemIndex)
import qualified Data.List as List
import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ(Doc,text,int,(<>),(<+>),($$),($+$),render)
import Data.Functor.Identity

import Data.Typeable




-- import Debug.Trace
-- ttx s x = trace (s++show x) x

-- A (FiniteSet a) is mapping from a list of literals to "a".

data FiniteSet a = 
     FA [Dimension]             -- The n dimensions.
        (DM.Map Key a)          -- Mapping of Tuples to value.
 
data SetI t where
  BoolI :: SetI Bool
  PropI :: SetI (Prop Int)
  BddI :: SetI (Bdd Int)
  
instance Show (SetI x) where
  show BoolI = "Bool set"
  show PropI = "Prop set"
  show BddI =  "Bdd  set"

  
------------------------------------------------------
-- operations to extract information from a FiniteSet

dims:: FiniteSet a -> [Int]
dims (FA ds map) = List.map dimSize ds

dimFA (FA ds arr) = ds
sizeFA (r@(FA ds arr)) = product (dims r)
countFA (FA ds set) = size set

varsFS:: Ord a => FiniteSet (Prop a) -> [a]
varsFS (FA ds s) =  nub $ concat (map (letters . snd) (DM.toList s))

-----------------------------------------------
-- some useful monadic operations on MyMap maps

-- This is the bottle neck. What can we do?
-- The same keys are used, but the combining thing
-- is monadic so we can't use any of the specialized code from the library.

{-
combineInsert:: (Monad m, Ord k) =>
     (a -> a -> m a) -> [(k, a)] -> Map k a -> m (Map k a)
combineInsert f [] set = return set
combineInsert f ((x@(key,b1)):xs) set =
   case DM.lookup key set of
     Just b2 -> do { b3 <- f b1 b2; combineInsert f xs (DM.insert key b3 set)}
     Nothing -> combineInsert f xs (DM.insert key b1 set)


fromListWithM:: (Monad m, Ord k) => 
    (a -> a -> m a) -> [(k, a)] -> m (Map k a)
fromListWithM disj us = combineInsert disj us DM.empty
-}

foldlSetM :: 
  Monad m => (key -> x -> ans -> m ans) -> 
             ans -> Map key x -> m ans
foldlSetM g base set = foldlM acc base (DM.toList set)
  where acc (key,x) ans = g key x ans     

------------------------------------------------------------------
-- Creating Finite Sets where the programmer supplies every tuple.

fromLitList:: v -> [Dimension] -> [[Literal]] -> FiniteSet v
fromLitList v ds xs | all ok xs = FA ds (fromList (map f xs))
  where f x = (toKey "fromLitList" ds x,v)
        ok x = length x == length ds
fromLitList v ds xs = 
  error ("The dimensions "++showDimL ds++
         " are not the right width for the tuple: "++
         show xs++indentL ds)

manyL:: TPD v => [Dimension] -> [[Literal]] -> FiniteSet v
manyL ds xs = (fromLitList true ds xs) 

fromFiniteList ::
  Finite a => b -> [Dimension] -> [a] -> FiniteSet b
fromFiniteList true ds xs = FA ds (fromList (map f xs))
  where f t = (toKey ("fromFiniteList "++show ds++" "++show t)
                      ds (toLit t),true)
  
manyD:: (TPD b, Finite a) => [Dimension] -> [a] -> (FiniteSet b)
manyD ds xs = (fromFiniteList true ds xs)

fromFList :: Finite a => [Dimension] -> [(a,b)] -> FiniteSet b
fromFList ds xs = FA ds (fromList (map f xs))
   where f (t,x) = (toKey "fromFList" ds (toLit t),x)

------------------------------------------------------------------
-- Creating Finite Sets where the programmer chooses some tuples
-- from all possible tuples derived from a list of dimensions.

partialByLit:: [Dimension] -> ([Literal] -> Maybe a) -> FiniteSet a
partialByLit dims pred = 
   FA dims
      (fromList [ (i,j) 
                | i <- allKeys dims
                , Just j <- [pred (tuple i)] ])                 

universe :: [Dimension] -> ([Literal] -> a) -> FiniteSet a 
universe ds f = partialByLit ds (Just . f)                                           

always:: a -> Maybe(Prop Int)
always x = Just(TruthP)
  
enumPoly:: ([Literal] -> Int -> Maybe (Int, t)) -> [Dimension] -> Int -> (Int, FiniteSet t)                     
enumPoly f ds seed = enumByKey g ds seed
   where g key = f (tuple key)
                               
-- enumByKey:: (Key -> Int -> Maybe (Int, t)) -> [Dimension] -> Int -> (Int, FiniteSet t)                     
-- Linear assignment
enumByKey pred dims seed = finish (walk seed (allKeys dims))
  where finish (last,zs) = (last,FA dims (fromList zs))
        walk next [] = (next,[])
        walk next (key:is) = case pred key next of 
                             Nothing -> walk next is
                             Just(new,j) -> (final,(key,j):ys)
                                where (final,ys) = walk new is 
-- enumByKey False pred dms seed =                                 
                
count xs n = Just(n+1,LetterP n)                                

-- This is used to initialize     low .. high

countWithBounds:: (DM.Map Key Bool, DM.Map Key Bool) -> Key -> Int -> Maybe (Int, Prop Int)
countWithBounds (lowv,highv) indexes next =
  case (elemFA indexes lowv,elemFA indexes highv) of
   (True,True) -> Just(next,true)                 -- in the small set
   (False,True) -> Just(next +1,LetterP next) -- in just the big set
   (True,False) -> error ("Inconsistent bounds. The lower bound\n  "++show lowv++"\nShould be a subset of the upperbound\n   "++show highv)
   (False,False) -> Nothing -- Just(next,AbsurdP)   -- Not in either

elemFA lits set = 
  case DM.lookup lits set of
    Nothing -> False
    Just t -> t

-------------------------------------------------
-- Logartihmic encoding is ONLY useful when at most
-- one of a set is possible!!!!
-- A Log assignment introduces Log (base 2) N  
-- new propositional variables, and each item in
-- the universe is represented as a formula over 
-- those variables. Basically a Base 2 number.
-- Suppose (Log N) = 3, then the 0th item has
-- formula (False,False,False) = (not x1, not x2, not x3)
-- the 1st (False,False,False) = (not z1, not x2, x3)

numbits:: Int -> Int
numbits n = ceiling (logBase 2 (fromIntegral n))

-- introduce some new variables to represent each of
-- the items from "xs", start at (LetterP i), and 
-- return a mapping from each item to its formula, also
-- teturn the next available index for a new LetterP
-- initial "abcd" 7   ----> 
--    ([('a',[~p7,~p8]),('b',[~p7,p8]),('c',[p7,~p8]),('d',[p7,p8])],9)

initial :: [a] -> Int -> ([(a, [Prop Int])], Int)
initial xs i = (zip xs (reverse (g i)),i+n)
  where n = numbits (length xs)
        g:: Int -> [[Prop Int]]
        g m | m > (n+i-1) = [[]]
        g n = map (LetterP n:) ys ++ map ((NotP (LetterP n)):) ys
           where ys =  (g (n+1))

incWithCarry (NotP(LetterP x)) = (AbsurdP,LetterP x)
incWithCarry (LetterP x)       = (TruthP,NotP(LetterP x))

incWithCarryL :: [Prop n] -> (Prop n,[Prop n])
incWithCarryL [x] = (carry,[bit]) where (carry,bit) = incWithCarry x
incWithCarryL (x:xs) = 
  case incWithCarryL xs of
    (AbsurdP,ys) -> (AbsurdP,x:ys)
    (TruthP,ys) -> let (carry,y) = incWithCarry x
                   in (carry,y:ys)

zs::[Prop Int]                   
zs = [NotP (LetterP 0),LetterP 1,NotP(LetterP 2)]                   

inc xs = case incWithCarryL xs of
           (AbsurdP,ys) -> ys
           (TruthP,ys) -> ys -- error ("overflow in increment "++show xs++" -> "++show ys)
   
---------------------------------------------------------------------
-- Creating some specific FiniteSets from other FiniteSets
-- Relation is a FiniteSet of (Prop Int)
-- any index entry not populated, is assumed to be "AbsurdP"

type Relation = FiniteSet (Prop Int)  

emptyRel :: [Dimension] -> FiniteSet a  
emptyRel dims = FA dims DM.empty

fullRel :: TPD a => [Dimension] -> (FiniteSet a)
fullRel ds = (universe ds (\ _ -> true))

equal2 :: (TPD a) => [Dimension] -> (FiniteSet a)
equal2 [d1,d2] | d1==d2 = (FA [d1,d2] (DM.fromList (List.foldr acc [] list)))
  where -- f (k@(Key _ [x,y])) = ((k,true),x==y)
        f key | length ls == 2 = ((key,true),ls!!0 == ls!!1)
           where ls = tuple key
        acc (p,True) ans = p:ans
        acc (p,False) ans = ans
        list = map f (allKeys [d1,d2])
    
equalFA :: (TPD a) => FiniteSet t -> (FiniteSet a)
equalFA (FA ds _) = equal2 ds

--------------------------------------------------------------------
-- Simple logical operations on FiniteSets are implemented pointwise
-- and represent the set operations.

-- in the function "add" if a result ends up "false",  throw it away, 
-- since it will be represented by the default value of "false"
add (key,x) ans = return(if isFalse x then ans else ((key,x):ans))

scan1:: BooleanM m b => (b -> m b) -> [(Key,b)] -> [Key] -> m [(Key,b)]
scan1 g xs all = 
  do { let f = false 
     ; let absent [] = return []
           absent (key:ys) = do { x <- g f; zs <- absent ys; add (key,x) zs}
           merge [] all = absent all
           merge (x:xs) [] = error ("Bad list in complement")
           merge (ps@((x,b):xs)) (ls@(y:ys))
             | x==y = do { b2 <- g b; zs <- merge xs ys; add (x,b2) zs }
             | x<y  = do { b2 <- g b; zs <- merge xs ls; add (x,b2) zs }
             | x>y  = do { b2 <- g f; zs <- merge ps ys; add (y,b2) zs }
     ; merge xs all}


-- scan2 is a binary scan down two Relations, using special cases (g01 and g10) 
-- to handle the missing indices that are not stored because they are AbsurdP.
     
scan2 g11 g01 g10 (FA ds1 x) (FA ds2 y) | ds1 /= ds2 = error ("Dimensions don't match in scan2.")
scan2 g11 g01 g10 (FA ds1 x) (FA ds2 y) = 
  do { let out2 [] = return []
           out2 (x:xs) = do { y <- g10 x; ys <- out2 xs; add y ys}
           out1 [] = return []
           out1 (x:xs) = do { y <- g01 x; ys <- out1 xs; add y ys}           
           merge [] ys = out1 ys
           merge xs [] = out2 xs
           merge (ps@((x,b):xs)) (ls@((y,c):ys))
             | x==y = do { b2 <- g11 b c;      zs <- merge xs ys; add (x,b2) zs }
             | x<y  = do {(_,b2) <- g10 (x,b); zs <- merge xs ls; add (x,b2) zs }
             | x>y  = do {(_,b2) <- g01 (y,c); zs <- merge ps ys; add (y,b2) zs }
           xs = toAscList x
           ys = toAscList y
     ; list <- merge xs ys
     ; return(FA ds1 (fromList list))}     


-----------------------
-- Set based operations
     
complement :: BooleanM m b => FiniteSet b -> m (FiniteSet b)
complement (FA ds set) = 
  do { let list = toAscList set
           all = allKeys ds
     ; list2 <- scan1 negM list all
     ; return(FA ds (fromList list2))
     }

intersect :: BooleanM m b => FiniteSet b -> FiniteSet b -> m(FiniteSet b)
intersect x y = scan2 conjM notThere notThere x y
  where notThere (lits,b) = return(lits,false)
  
union :: BooleanM m b => FiniteSet b -> FiniteSet b -> m(FiniteSet b)
union x y = scan2 disjM there there x y
  where there (lits,b) = return(lits,b)
  
difference:: BooleanM m b => FiniteSet b -> FiniteSet b -> m (FiniteSet b)
difference x y = do { z <- (complement y); intersect x z }

--------------------------------
-- relational algebra operations

-- Projection.  Projection means several tuples may be merged
-- into one, when their differences are projected away. The resulting
-- tuple will be the disjunction of all the original tuples propositions.

project :: (BooleanM m a) => [Int] -> FiniteSet a -> m(FiniteSet a)
project cols (r@(FA dims table)) | any bad cols = error ("Bad projection columns "++show cols++" don't match with dimensions "++showDimL dims)
  where bad x = x < 0 || x >= length dims
project cols (r@(FA dims table)) | cols == [0 .. length dims - 1] = return r
project cols (r@(FA dims table)) = 
   do { let newdims = fmap (dims !!) cols
            tab2 = toList table
            adjust (key,bool) = (keyProj dims cols key,bool)
          --  adjust (337 _ lits,bool) = (newkey,bool)
          --        where newkey = toKey "project" newdims (map (lits !!) cols)
      ; tab3 <- fromListWithM disjM (map adjust tab2)
      ; return (FA newdims tab3) }    
         
select:: (TPD t) => (Key -> Bool) -> FiniteSet t -> FiniteSet t                                                              
select testp (r@(FA dims tab)) = FA dims (DM.foldrWithKey acc DM.empty tab)
  where acc key b ans = 
              case (testp key,b) of
                (True,x) | isFalse x -> ans
                (True,b) -> DM.insert key b ans
                other -> ans   
                             
selectM:: (Monad m,TPD t) => (Key -> m Bool) -> FiniteSet t -> m (FiniteSet t)
selectM testp (FA dims tab) = 
      do { set <- foldlSetM acc DM.empty tab; return(FA dims set)}
   where acc key x ans = 
           do { b <- testp key
              ; return(if b then (DM.insert key x ans) else ans)}

join:: (BooleanM m a) => Int -> FiniteSet a -> FiniteSet a -> m(FiniteSet a)
join n (FA dim1 tab1) (FA dim2 tab2) | take n dim1 /= take n dim2 = error "Bad join"
join n (FA dim1 tab1) (FA dim2 tab2) =    
  do { let dim3 = dim1 ++ drop n dim2
           l1 = toAscList tab1
           l2 = toAscList tab2
     ; ws <- joinScan dim3 n l1 l2
     ; tab3 <- fromListWithM disjM ws 
     ; return (FA dim3 tab3) }
 
{- 
-- "matching n xs ys" compare the first "n" literals in two Keys
-- This has a bug, since the order of keys in the set might
-- not be the natural order.
matching :: Int -> Key -> Key -> Ordering
matching n (Key _ xs) (Key _ ys) = help n xs ys where
   help 0 xs ys = EQ
   help 1 (x:xs) (y:ys) = compare x y
   help n (x:xs) (y:ys) | x==y = help (n-1) xs ys
                        | otherwise = compare x y
   help _ [] (y:ys) = LT
   help _ (x:xs) [] = GT
   help _ [] [] = EQ
-}

splitR :: (Key -> Ordering) ->  [(Key,b)] -> (  [([Literal],b)],  [(Key,b)])
splitR matches [] = ([],[])
splitR matches (ys@((key,v):xs)) =
       case matches key of
          EQ  -> let (matching,others) = splitR matches xs
                 in ((tuple key,v):matching,others)
          otherwise ->  ([],ys)

joinScan:: BooleanM m b => [Dimension] -> Int -> [(Key,b)] -> [(Key,b)] -> m [(Key,b)] 
joinScan ds n (xs@((k1,v1):ms)) (ys@((k2,v2):ns)) =
  case (compareKey n k1 k2) of
    EQ -> let (prefix1,suffix1) = splitR (compareKey n k1) xs
              (prefix2,suffix2) = splitR (compareKey n k2) ys
              pairs = [ (toKey "joinScan" ds (tupk1 ++ drop n tupk2),conjM v1 v2)   
                      | (tupk1,v1) <- prefix1
                      , (tupk2,v2) <- prefix2 ]
              sequence [] = return []
              sequence ((key,comp):xs) = do { b <- comp; ys <- sequence xs; return((key,b):ys)}
          in do { ws <- sequence pairs
                ; ys <- joinScan ds n suffix1 suffix2
                ; return(ws++ys)}
    LT -> joinScan ds n ms ys
    GT -> joinScan ds n xs ns
joinScan ds n _ _ = return []        
     

----------------------------------------------------
-- Compute a proposition that is inhabited if some 
-- property is true on a Finite Set.

scanM f base [] = return base
scanM f y ((key,x):xs) = do { z <- f x y; scanM f z xs}

scanFiniteSetM f base (FA dims set) = scanM f base (toList set)

full :: BooleanM m b => FiniteSet b -> m b
full r = scanFiniteSetM conjM true r

some :: BooleanM m b => FiniteSet b -> m b
some r = scanFiniteSetM disjM false r

none :: BooleanM m b => FiniteSet b -> m b
none r = scanFiniteSetM acc true r
   where acc x y = do { z <- negM x; conjM z y }


-- "one" can be quite expensive in that it creates large Propositions
one (FA dims set) = disjoin pairs false
   where list = toList set
         pairs = exactlyOne list list
         conjoin [] ans = return ans
         conjoin (Left x:xs) y = do { z <- conjM x y; conjoin xs z}
         conjoin (Right x:xs) y = do { w <- negM x; z <- conjM w y; conjoin xs z}
         disjoin [] ans = return ans
         disjoin ((key,x):xs) y = do { w <- conjoin x true; z <- disjM w y; disjoin xs z}

-- it works by negating (Right) all but one element of a list
exactlyOne xs ys = [(i,[ if i == j then Left b else (Right c) 
                       | (j,c) <- ys]) 
                   | (i,b) <- xs]

--  here's quick test
q12:: [(Char,[Either (Prop Int) (Prop Int)])]
q12 = exactlyOne [('a',1),('b',2),('c',3)] [('a',1),('b',2),('c',3)]

-- Is one Finite Set a subset of another Finite Set?

subsetM:: BooleanM m b => FiniteSet b -> FiniteSet b -> m b
subsetM (FA ds1 _) (FA ds2 _) | ds1 /= ds2 = error ("different domains in subset")
subsetM xs ys =
  do { let g01 (k,x) = return(k,true)                 -- (Imply False x)
           g10 (k,x) = do { z <- negM x; return(k,z)} -- (Imply x False)
     ; FA ds set <- scan2 implyM g01 g10 xs ys
     ; scanM conjM true (toList set)
     }

-----------------------------------------------------------------------
-- functional dependency can be expressed as a self join and then a 
-- selection of tuples where the range columns don't match. If
-- that relation is empty (the 'none' predicate) then there is 
-- a functional dependency. (funDep dom rng) means that the
-- columns 'dom' functional determine the columns 'rng'
-- I.e.
-- funDep [0,1] [2] x = none(select (colsDiff [(2,3)]) 
--                                  (join 2 (project [0,1,2] x)(project [0,1,2] x)))
-- Note that the selfjoin has columns [0,1,2,2]  
--                      with indices   0,1,2,3 , so we need columns (2,3) not to match
-- Another example
-- funDep [1] [2,0] x = none(select (colsDiff [(1,3),(2,4)]) 
--                                  (join 1 (project [1,2,0] x)(project [1,2,0] x)))
-- Note that the selfjoin has columns [0,2,0,2,0]  
--                      with indices   0,1,2,3,4 so we need columns (1,3) and (2,4) not to match

bad dims x = (x < 0 || x >= length dims) 

-- funDep:: (BooleanM m b) => [Int] -> [Int] -> FiniteSet b -> m b
funDep dom rng (r@(FA dims table)) | any (bad dims) dom  = 
     error ("Domain indices "++show dom++" not in dimension range 0.."++show (length dims -1))
funDep dom rng (r@(FA dims table)) | any (bad dims) rng = 
     error ("Range indices "++show rng++" not in dimension range 0.."++show (length dims -1))            
funDep dom rng r | not(List.null(List.intersect dom rng)) =
     error ("Domain "++show dom++" and Range "++show rng++" are not disjoint.")

funDep dom rng (r@(FA dims table)) = 
    do { r2 <- project (dom++rng) r
       ; r3 <- (join (length dom) r2 r2)
       ; r4 <- none(select (not . colsSame rngIndices) r3)
       ; putStrLn ("\n###FunDEP\n"++show r4)
       ; return r4}
  where 
        rngIndices = [(length dom + i, length dom + length rng + i) | i <- [0..length rng - 1]]
        -- colsDiff [] xs = False
        -- colsDiff ((i,j):more) xs = (xs!!i)/=(xs!!j) || colsDiff more xs
        colsSame [] key = True
        colsSame ((i,j):more) key = ((tuple key)!!i)==((tuple key)!!j) && colsSame more key


-----------------------------------------------------

boolToProp :: FiniteSet Bool -> FiniteSet (Prop Int)
boolToProp (FA ds table) = FA ds (DM.map f table)
  where f True = TruthP
        f False = AbsurdP
        
boolToBdd :: FiniteSet Bool -> FiniteSet (Bdd Int)
boolToBdd (FA ds table) = FA ds (DM.map f table)
  where f True = trueBdd
        f False = falseBdd
        
propToBdd:: FiniteSet (Prop Int) -> IO (FiniteSet (Bdd Int))
propToBdd (FA ds table) = do { zs <- seq list; return(FA ds (DM.fromList zs))}
  where list = DM.toList table
        seq [] = return []
        seq ((key,prop):xs) =
             do { bdd <- prop2Bdd prop
                ; ys <- seq xs
                ; return((key,bdd):ys)}
    

                      



----------------------------------------------------------------------
-- evaluation of Propositions using valuation functions in the 
-- form returned by minisat, a list of Int where a negative number
-- means a negated literal.

-- eval2 :: (Eq a) => (Int -> Prop a) -> Prop Int -> [Int] -> Prop a
eval2 f (AndP x y) assign     = andP (eval2 f x assign) (eval2 f y assign)   
eval2 f (OrP x y) assign      = orP (eval2 f x assign) (eval2 f y assign)   
eval2 f (ImpliesP x y) assign = implyP (eval2 f x assign) (eval2 f y assign)  
eval2 f (NotP x) assign       = notP (eval2 f x assign)
eval2 f TruthP assign         = TruthP
eval2 f AbsurdP assign        = AbsurdP
eval2 f (LetterP x) assign    = case find ok assign of { Nothing -> f x; Just x -> sign x}
  where ok y = abs y == x
        sign x | x <= 0 = AbsurdP
               |x > 0   = TruthP              

-- instantiate assign r = unary f r
--   where f n x = eval2 LetterP x assign  

-- unary:: (Boolean b1, Boolean b2) =>
--       (Key -> b1 -> b2) -> FiniteSet b1 -> FiniteSet b2
unary f (FA ds set) = FA ds (DM.filter id (DM.mapWithKey f set))

instan:: [Int] -> FiniteSet (Prop Int) -> FiniteSet Bool
instan minisatSol r = (unary f r)
  where subst = map toPair minisatSol
        sign x | x <= 0 = False
               |x > 0   = True
        toPair x = (abs x,sign x)
        f n x = eval subst True x
        

  

------------------------------------------------------------------
-- Class magic for building Finite Sets from Finite elements 
-- or Tuples of Finite elements. See the function manyD
--------------------------------------------------------

class (Typeable a,Read a,Show a) => Finite a where
  toLit:: a -> [Literal]
  toLit x = [toLiteral x]
  fromLit:: [Literal] -> a
  fromLit [LCon _ x] = read x

-- Most types except tuples can use the default interpretation of toLit
-- Only enumerations, t, in which are instances of (Read t) and (Show  t)
-- should use the default interpretations of fromLit 


-- Finite instances

instance Read Literal where  
instance Finite [Literal] where
  toLit x = x
  fromLit x = x
  
instance Finite Int where  
  toLit x = [LInt x]
  fromLit [LInt x] = x
instance Finite String where
  toLit x = [LString x]
  fromLit [LString x] = x
instance Finite Double where  
  toLit x = [LDouble x]
  fromLit [LDouble x] = x
instance Finite Char where  
  toLit x = [LChar x]
  fromLit [LChar x] = x  
instance Finite Bool where 

 
-- Tuples are Finite if their components are
instance (Finite a,Finite b) => Finite (a,b) where                      
  toLit (x,y) = toLit x ++ toLit y
  fromLit [x,y] = (fromLit [x],fromLit [y])

instance (Finite a,Finite b,Finite c) => Finite (a,b,c) where
  toLit (x,y,z) = toLit x ++ toLit y ++ toLit z
  fromLit [x,y,z] = (fromLit [x],fromLit [y],fromLit [z])  

              
             
                    
----------------------------------------------------------  
-- instances

instance Eq Dimension where
  (Dim n b1 xs) == (Dim m b2 ys) = n==m && b1==b2 && xs==ys
                                                                                                                            
instance Eq a => Eq(FiniteSet a) where
  (FA ds1 m1) == (FA ds2 m2) =  DM.size m1 == DM.size m2 && ds1==ds2 && m1==m2

-- Show instances 

indentL xs = concat(map f xs)
  where f x = "\n   "++dimName x
  
instance PP Int where
  pp x = PP.int x
instance PP String where
  pp x = text x
instance PP Char where
  pp x = PP.char x

ppFA :: PP a => (Doc -> Doc) -> ([Doc] -> a1) -> FiniteSet a -> a1
ppFA g cat (FA [] map) =
  case (DM.toAscList map) of
    [(i,x)] -> cat [text "Scalar ",pp x]
    other -> error ("Badly formed zero dimensional table")
ppFA g cat (FA dims map) = cat (text (dimNameL dims) :(fmap widen (DM.toAscList map)))
  where widen (k,x) = toDoc(k,pp x)
        toDoc (key,str) = text (plistf show "(" (tuple key) "," (")"++show (flat key))) <> g str

       
ppDim :: [Int] -> Doc
ppDim xs = PP.parens(commafy (fmap (text . show) xs))

commafy [] = PP.empty
commafy [x] = x
commafy (x:xs) = x <> text "," <> commafy xs

instance PP (FiniteSet Bool) where
  pp (x@(FA dims tab)) = if DM.null tab then text "{}" else (ppFA g f x)  
    where f (dimDoc : docs) = dimDoc $$ PP.braces(PP.fsep docs)
          g x = PP.empty

instance PP (FiniteSet (Prop Int)) where
  pp (x@(FA dims tab)) = if DM.null tab then text "{}" else (ppFA g f x)  
    where f (dimDoc : docs) = dimDoc $$ PP.braces(PP.fsep (PP.punctuate (text ",") docs))
          g x = text "=" <> x     
 
instance Show (FiniteSet Bool) where
  show (x@(FA dims tab)) = render(pp x)

instance Show (FiniteSet (Prop Int)) where
  show (x@(FA dims tab)) = render(pp x)
  
instance Show (FiniteSet Int) where
  show (x@(FA dims tab)) = if DM.null tab then "{}" else render (ppFA g f x)  
    where f (dimDoc : docs) = dimDoc $$ PP.braces(PP.fsep docs)
          g x = text "=" <> x   
          
instance Show (FiniteSet Char) where
  show (x@(FA dims tab)) = if DM.null tab then "{}" else render (ppFA g f x)  
    where f (dimDoc : docs) = dimDoc $$ PP.braces(PP.fsep docs)
          g x = text "=" <> x             
 


-----------------------------------------------------------------
-- simple data to test things with


x1,x2:: Relation
x1 = partialByLit (expand[3,3]) (\ xs -> if sum xs == 3 then Just(LetterP 3) else Nothing)
x2 = partialByLit (expand[3,3]) (\ xs -> if sum xs == 2 then Just TruthP else Nothing)

props = [p3,OrP p1 p5,TruthP,p4,NotP p3,p4,AbsurdP,NotP (AndP p4 p5),AndP p2 p7]

minus1,plus1 :: FiniteSet (Prop Int)
plus1 = fromFiniteList true (expand [10,10]) [(i::Int,i+1) | i <- [0..8]]
minus1 = runIdentity (project [1,0] plus1)
plus2 = runIdentity $ (join 1 plus1 minus1) >>= project [2,1]
plusEvens = select (\ key -> even (un((tuple key)!!1))) plus1
  where un (LInt x) = x
stuff = runIdentity (union plus1 minus1)
others = runIdentity $ complement plus1

full4By4 = partialByLit (expand[4,4]) always

(next,triples) = enumPoly f (expand[4,4,4]) (99::Int)
   where f [LInt i,j,k] n = if even i then Just(n+1,LetterP n) else Nothing
zeroth = runIdentity $ project [0] triples
first = runIdentity $ project [1] triples
col12 = runIdentity $ project [1,2] triples

-- some examples
(mm,set1) = enumPoly f [dim 3, dim 3] 0
  where f ls 3 = Just(4,TruthP)
        f ls i = if i < 5 then Just(i+1,LetterP i) else Nothing

(_,set2) = enumPoly f [dim 3, dim 3] mm
  where f [LInt 1,LInt 2] i = Nothing
        f ls i = Just(i+1,LetterP i)
        
(zjj,set3) = enumPoly f [dim 3, dim 3] 0
  where f ls i = Just(i+1,LetterP i)
  
(_,set4) = enumPoly f [dim 3, dim 3] zjj
  where f ls 12 = Just(13,TruthP)
        f ls i  = Just(i+1,LetterP i)  
        
set5 = fromFList [dim 3, dim 3] [((1,2)::(Int,Int),TruthP),((0,1),TruthP ::Prop Int)]
        
        
people = ["Anita","Barbara","Caleb","Frank","George","Margareet","Tim","Walter"]
pd = dimS people  -- A dimension
tuples = [ ("Frank","Tim"),("Tim" , "Caleb"),("Walter","Frank"), 
   ("Anita","Tim"),("Margareet","Barbara"),("Barbara","Caleb")]

parent:: FiniteSet Bool
parent = fromFiniteList True [pd,pd] tuples
person:: FiniteSet Bool
person = fromFiniteList True [pd] people
  
          
 