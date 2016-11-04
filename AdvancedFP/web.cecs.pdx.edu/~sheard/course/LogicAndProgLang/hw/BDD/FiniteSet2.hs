-- This file is a simple version of FiniteSet.hs modified
-- from the Funlog system. 

{-# LANGUAGE FlexibleContexts, TypeSynonymInstances, 
    DeriveDataTypeable, FlexibleInstances,
    RankNTypes, GADTs, ScopedTypeVariables  #-}

module FiniteSet2 where
import Prop
import Boolean2 
import BddIO hiding (toLit,neg)
import Auxfuns(plistf)
import Literal
                    
import qualified Data.Map as DM 
import Data.Map hiding (map,filter,union,difference)
import Data.List(nub,find,elemIndex)
import qualified Data.List as List
import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ(Doc,text,int,(<>),(<+>),($$),($+$),render)
import Data.Functor.Identity

import Data.Typeable

-- import Debug.Trace
-- ttx s x = trace (s++show x) x

-- A Dimension is a finite set of some Base type.
-- Invariant that (Dim n b xs) => length xs == n
-- We store the elements of the set as a [Literal]
-- The order in the list assigns an Int to each element.

data Dimension = Dim Int Base [Literal] 
dimSize (Dim n base ls) = n
dimElem (Dim n base ls) = ls

-- A (FiniteSet a) is mapping from a list of literals to "a".

data FiniteSet a = 
     FA [Dimension]             -- The n dimensions.
        (DM.Map [Literal] a)    -- Mapping of Tuples to value.
 
data SetI t where
  BoolI :: SetI Bool
  PropI :: SetI (Prop Int)
  BddI :: SetI (Bdd Int)
  
----------------------------------------------------------------------
-- Dimensions

-- For printing parts of dimensions
dimName (Dim n base ls) =  show base++"#"++show n
dimNameAll (Dim n base (l:ls)) = show base++"#"++show(l:ls)
dimNameL ds = plistf dimName "(" ds "," ")"
showDimSize (Dim n base xs) = show n
showDimL ds = show(map dimName ds)

-- We can create Dimensions a number of ways.
-- State the size, and we generate a mapping [0 .. n]

dim :: Int -> Dimension
dim n = Dim n Int [LInt x | x <- [0 .. n-1]]

-- Exhibit a list of strings that describes the mapping
dimS:: [String] -> Dimension
dimS xs = Dim (length xs) String (map LString xs)

-- Exhibit a list of anything that can be turned into a string.
dimL :: (Show a) => [a] -> Dimension
dimL xs = Dim (length xs) String (map (LString . show) xs)

-- Any element of a type in the Enum class describes a dimension
-- over the finite set described by the full enumeration.
dimE :: (Show a,Enum a) => String -> a -> Dimension
dimE name x = Dim (length xs) (Enum name) (map (LCon name . show) xs)
  where first = (toEnum 0)
        theyHaveTheSameType = [first,x]
        xs = enumFrom first

-- Create a multi-Dimensional mapping where all the individual 
-- mappings are over a 0-based range of Int.
expand :: [Int] -> [Dimension]
expand xs = map dim xs

------------------------------------------------------------------------------------
dims:: FiniteSet a -> [Int]
dims (FA ds map) = List.map dimSize ds
dimFA (FA ds arr) = ds
sizeFA (r@(FA ds arr)) = product (dims r)
countFA (FA ds set) = size set

vars:: Ord a => FiniteSet (Prop a) -> [a]
vars (FA ds s) =  nub $ concat (map (letters . snd) (Data.Map.toList s))

------------------------------------------------------------------
-- Creating Finite Sets where the programmer supplies every tuple.

fromLitList:: v -> [Dimension] -> [[Literal]] -> FiniteSet v
fromLitList v ds xs | all ok xs = FA ds (fromList (map f xs))
  where f x = (x,v)
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
  where f t = (toLit t,true)

manyD:: (TPD b, Finite a) => [Dimension] -> [a] -> (FiniteSet b)
manyD ds xs = (fromFiniteList true ds xs)


fromFList :: Finite a => [Dimension] -> [(a,b)] -> FiniteSet b
fromFList ds xs = FA ds (fromList (map f xs))
   where f (t,x) = (toLit t,x)

------------------------------------------------------------------
-- Creating Finite Sets where the programmer chooses some tuples
-- from all possible tuples derived from a list of dimensions.

allTuples:: [Dimension] -> [[Literal]]
allTuples [] = []
allTuples [d] = map (:[]) (dimElem d)
allTuples (d:ds) = [(x:xs) | x<- dimElem d, xs <- allTuples ds]


partialByLit:: [Dimension] -> ([Literal] -> Maybe a) -> FiniteSet a
partialByLit dims pred = 
   FA dims
      (fromList [ (i,j) 
                | i <- allTuples dims
                , Just j <- [pred i] ])                 

universe :: [Dimension] -> ([Literal] -> a) -> FiniteSet a 
universe ds f = partialByLit ds (Just . f)                                           

always:: a -> Maybe(Prop Int)
always x = Just(TruthP)
  
enumPoly:: ([Literal] -> a -> Maybe (a, t)) -> [Dimension] -> a -> (a, FiniteSet t)                     
enumPoly pred dims seed = finish (walk seed (allTuples dims))
  where finish (last,zs) = (last,FA dims (fromList zs))
        walk next [] = (next,[])
        walk next (i:is) = case pred i next of 
                             Nothing -> walk next is
                             Just(new,j) -> (final,(i,j):ys)
                                where (final,ys) = walk new is      
                               
enum:: ([Literal] -> Int -> Maybe (Int, t)) -> [Dimension] -> Int -> (Int, FiniteSet t)                     
enum = enumPoly                                 
                
count xs n = Just(n+1,LetterP n)                                

-- This is used to initialize     low .. high
countWithBounds (lowv,highv) indexes next =
  case (elemFA indexes lowv,elemFA indexes highv) of
   (True,True) -> Just(next,TruthP)                 -- in the small set
   (False,True) -> Just(next+(1::Int),LetterP next) -- in just the big set
   (True,False) -> error ("Inconsistent bounds. The lower bound\n  "++show lowv++"\nShould be a subset of the upperbound\n   "++show highv)
   (False,False) -> Nothing -- Just(next,AbsurdP)   -- Not in either

elemFA lits set = 
  case DM.lookup lits set of
    Nothing -> False
    Just t -> t
   
---------------------------------------------------------------------
-- Creating some specific FiniteSets from other FiniteSets
-- Relation is a FiniteSet of (Prop Int)
-- any index entry not populated, is assumed to be "AbsurdP"

type Relation = FiniteSet (Prop Int)  

emptyRel :: [Dimension] -> FiniteSet a  
emptyRel dims = FA dims Data.Map.empty

fullRel :: TPD a => [Dimension] -> (FiniteSet a)
fullRel ds = (universe ds (\ _ -> true))

equal2 :: (TPD a) => [Dimension] -> (FiniteSet a)
equal2 [d1,d2] | d1==d2 = (FA [d1,d2] (DM.fromList (List.foldr acc [] list)))
  where f [x,y] = (([x,y],true),x==y)
        acc (p,True) ans = p:ans
        acc (p,False) ans = ans
        list = map f (allTuples [d1,d2])
    
equalFA :: (TPD a) => FiniteSet t -> (FiniteSet a)
equalFA (FA ds _) = equal2 ds

--------------------------------------------------------------------
-- Simple logical operations on FiniteSets are implemented pointwise
-- and represent the set operations.

-- in the function "add" if a result ends up "false",  throw it away, 
-- since it will be represented by the default value of "false"
add (key,x) ans = return(if isFalse x then ans else ((key,x):ans))

scan1:: BooleanM m b => (b -> m b) -> [([Literal],b)] -> [[Literal]] -> m [([Literal],b)]
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
           all = allTuples ds
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
            adjust (key,bool) = (newkey,bool)
                  where newkey = map (key !!) cols
      ; tab3 <- combineInsert disjM (map adjust tab2) DM.empty
      ; return (FA newdims tab3) }    
      
combineInsert f [] set = return set
combineInsert f ((x@(key,b1)):xs) set =
   case DM.lookup key set of
     Just b2 -> do { b3 <- f b1 b2; combineInsert f xs (DM.insert key b3 set)}
     Nothing -> combineInsert f xs (DM.insert key b1 set)
      
select:: (TPD t) => ([Literal] -> Bool) -> FiniteSet t -> FiniteSet t                                                              
select testp (r@(FA dims tab)) = FA dims (DM.foldrWithKey acc DM.empty tab)
  where acc key b ans = case (testp key,b) of
                             (True,x) | isFalse x -> ans
                             (True,b) -> DM.insert key b ans
                             other -> ans                       

join:: (BooleanM m a) => Int -> FiniteSet a -> FiniteSet a -> m(FiniteSet a)
join n (FA dim1 tab1) (FA dim2 tab2) | take n dim1 /= take n dim2 = error "Bad join"
join n (FA dim1 tab1) (FA dim2 tab2) =    
  do { let dim3 = dim1 ++ drop n dim2
           l1 = toAscList tab1
           l2 = toAscList tab2
     ; ws <- joinScan n l1 l2
     ; tab3 <- combineInsert disjM ws (DM.empty)
     ; return (FA dim3 tab3) }
     
-- "matching n xs ys" compare the first "n" literals in two lists
matching :: (Ord a) => Int -> [a] -> [a] -> Ordering
matching 0 xs ys = EQ
matching 1 (x:xs) (y:ys) = compare x y
matching n (x:xs) (y:ys) | x==y = matching (n-1) xs ys
                         | otherwise = compare x y
matching _ [] (y:ys) = LT
matching _ (x:xs) [] = GT
matching _ [] [] = EQ

splitR matches [] = ([],[])
splitR matches (ys@((key,v):xs)) =
       case matches key of
          EQ  -> let (matching,others) = splitR matches xs
                 in ((key,v):matching,others)
          otherwise ->  ([],ys)

joinScan:: BooleanM m b => Int -> [([Literal],b)] -> [([Literal],b)] -> m [([Literal],b)] 
joinScan n (xs@((k1,v1):ms)) (ys@((k2,v2):ns)) =
  case (matching n k1 k2) of
    EQ -> let (prefix1,suffix1) = splitR (matching n k1) xs
              (prefix2,suffix2) = splitR (matching n k2) ys
              pairs = [ (tupk1 ++ drop n tupk2,conjM v1 v2)   
                      | (tupk1,v1) <- prefix1
                      , (tupk2,v2) <- prefix2 ]
              sequence [] = return []
              sequence ((key,comp):xs) = do { b <- comp; ys <- sequence xs; return((key,b):ys)}
          in do { ws <- sequence pairs; ys <- joinScan n suffix1 suffix2; return(ws++ys)}
    LT -> joinScan n ms ys
    GT -> joinScan n xs ns
joinScan n _ _ = return []        
     

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
subsetM xs ys =
  do { let g01 (k,x) = return(k,true)
           g10 (k,x) = return(k,x)
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

funDep:: (BooleanM m b) => [Int] -> [Int] -> FiniteSet b -> m b
funDep dom rng (r@(FA dims table)) | any (bad dims) dom  = 
     error ("Domain indices "++show dom++" not in dimension range 0.."++show (length dims -1))
funDep dom rng (r@(FA dims table)) | any (bad dims) rng = 
     error ("Range indices "++show rng++" not in dimension range 0.."++show (length dims -1))            
funDep dom rng r | not(List.null(List.intersect dom rng)) =
     error ("Domain "++show dom++" and Range "++show rng++" are not disjoint.")

funDep dom rng (r@(FA dims table)) = 
    do { r2 <- project (dom++rng) r
       ; r3 <- (join (length dom) r2 r2)
       ; none(select (not . colsSame rngIndices) r3) }
  where 
        rngIndices = [(length dom + i, length dom + length rng + i) | i <- [0..length rng - 1]]
        -- colsDiff [] xs = False
        -- colsDiff ((i,j):more) xs = (xs!!i)/=(xs!!j) || colsDiff more xs
        colsSame [] xs = True
        colsSame ((i,j):more) xs = (xs!!i)==(xs!!j) && colsSame more xs


-----------------------------------------------------

data NonStandard 
  = NSProp (Prop Int)
--  | NSYices ExpY
--  | NSMath (MExp Int)
--  | NSRel [Rel Int]

data Value 
   = VBase Literal
   | VFun (Value -> Value)
   | VFunM Integer (forall a. Value -> (Value -> IO a) -> IO a)
--   | VVector Dimension (Array Int Value)
--   | VMatrix Dimension Dimension (Array (Int,Int) Value)
   | forall i . (Boolean i,PP i) => VSet (SetI i) (FiniteSet i)
   | VTuple [Value]
   | VCon Int String String [Value]
   | VDomain [Dimension]
   | VNS NonStandard
  
instance Show Value where
  show v = "value"

class Encoding t where
   to     :: t -> Value 
   from   :: Value -> t
   
   
constToV :: (forall b . Boolean b => FiniteSet b -> b) -> Value
constToV f = VFun g
  where g (VSet BoolI set) = to (f set)
        g (VSet PropI set) =  VNS(NSProp (f set)) 
        g x = error ("Bad input to 'full'\n   "++show x)

{-


                        

-----------------------------------------
-- Creating non-standard functions


applyV :: (forall b . Boolean b => FiniteSet b -> FiniteSet b) -> Value -> Value
applyV f (VSet BoolI set) = VSet BoolI (f set)
applyV f (VSet PropI set) = VSet PropI (f set)
applyV f x = error ("Bad input to 'applyV'\n   "++show x)

fullV = constToV full
someV = constToV some
noneV = constToV none
oneV = constToV FiniteSet.one
projV xs = applyV (project xs)

subSetV = VFun g
  where g (VSet BoolI set1) = VFun (h set1)
        g (VSet PropI set1) = VFun (f set1)  
        g x = error ("Bad first input to 'subset'\n   "++show x)
        h :: FiniteSet Bool -> Value -> Value
        h set1 (VSet BoolI set2) = to (subset set1 set2)
        h set1 (VSet PropI set2) = VNS(NSProp (subset (liftFiniteSet set1) set2))
        h set1 x = error ("Bad second input to 'subset'\n   "++show x)
        f :: FiniteSet (Prop Int) -> Value -> Value
        f set1 (VSet PropI set2) = VNS(NSProp (subset set1 set2 ))
        f set1 (VSet BoolI set2) = VNS(NSProp (subset set1 (liftFiniteSet set2)))
        f set1 x = error ("Bad second input to 'subset'\n   "++show x)               


liftFiniteSet :: FiniteSet Bool -> FiniteSet (Prop Int)  
liftFiniteSet x = unary f x
  where f _ True = true
        f _ False = false
   


----------------------------------------------------------------------
-- evaluation of Propositions using valuation functions in the 
-- form returned by minisat, a list of Int where a negative number
-- means a negated literal.

-- eval2 :: (Eq a) => (Int -> Prop a) -> Prop Int -> [Int] -> Prop a
eval2 f (AndP x y) assign     = andB (eval2 f x assign) (eval2 f y assign)   
eval2 f (OrP x y) assign      = orB (eval2 f x assign) (eval2 f y assign)   
eval2 f (ImpliesP x y) assign = ImpliesP (eval2 f x assign) (eval2 f y assign)  
eval2 f (NotP x) assign       = notB (eval2 f x assign)
eval2 f TruthP assign         = TruthP
eval2 f AbsurdP assign        = AbsurdP
eval2 f (LetterP x) assign    = case find ok assign of { Nothing -> f x; Just x -> sign x}
  where ok y = abs y == x
        sign x | x <= 0 = AbsurdP
               |x > 0   = TruthP              

instantiate assign r = unary f r
  where f n x = eval2 LetterP x assign  

instan:: [Int] -> FiniteSet (Prop Int) -> FiniteSet Bool
instan minisatSol r = unary f r
  where subst = map toPair minisatSol
        sign x | x <= 0 = False
               |x > 0   = True
        toPair x = (abs x,sign x)
        f n x = eval subst True x

-}  

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
instance Show Dimension where
  show x = dimNameAll x

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
        toDoc (is,str) = text (plistf show "(" is "," ")") <> g str

       
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
plusEvens = select (\[LInt x,LInt y]-> even y) plus1
stuff = runIdentity (union plus1 minus1)
others = runIdentity $ complement plus1

full4By4 = partialByLit (expand[4,4]) always

(next,triples) = enum f (expand[4,4,4]) (99::Int)
   where f [LInt i,j,k] n = if even i then Just(n+1,LetterP n) else Nothing
zeroth = runIdentity $ project [0] triples
first = runIdentity $ project [1] triples
col12 = runIdentity $ project [1,2] triples

-- some examples
(mm,set1) = enum f [dim 3, dim 3] 0
  where f ls 3 = Just(4,TruthP)
        f ls i = if i < 5 then Just(i+1,LetterP i) else Nothing

(_,set2) = enum f [dim 3, dim 3] mm
  where f [LInt 1,LInt 2] i = Nothing
        f ls i = Just(i+1,LetterP i)
 