{-# LANGUAGE PatternGuards, GADTs, RankNTypes, 
    DeriveDataTypeable, FlexibleInstances #-}
module Value where

import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ(Doc,text,int,(<>),(<+>),($$),($+$),render)
import Monads(readLine)

import Prop
import Data.IORef(newIORef,readIORef,writeIORef,IORef)
import Data.List(elemIndex,nub,transpose)
import qualified Data.Map as DM 
import Data.Array

import Graphviz(pnG,Graph(..),Color(..),Shape(..),str2Color)
import Simple
import Minisat(solveClauses,negatedCl)
import UniqInteger(nextinteger)
import Debug.Trace
import YicesSyntax
import MathProg
import Data.Ratio
import Data.Typeable
  

-----------------------------------------------------------------

data Base = Int | String | Char | Double | Bool | Enum String | Tuple [Base]
  deriving Eq
  
data Literal 
   = LInt Int                  -- 5 , -5
   | LDouble Double            -- 2.3 , 0.5 , -3.4
   | LChar Char                -- 'x'
   | LString String            -- "\nabc"S@
   | LUnit
   | LCon String String        -- True, False, constants of enumerated types 
                               -- i.e. a type where ALL constructors are Nullary!
 deriving (Typeable)
------------------------------------------------
-- A Finite set has a 1-to-1 mapping with a prefix of the Integers
-- A Dimension captures such a mapping as a [Literal]. 

data Dimension = Dim Int Base [Literal] -- Invariant that (Dim n xs) => length xs == n
  
data FiniteSet a = 
     FA [Dimension]       -- The n dimensions.
        (DM.Map Int a)    -- Mapping of Row-major index to value.

------------------------------------------------------                                                                                                                                
                                                                                                                            
data Value 
   = VBase Literal
   | VFun (Value -> Value)
   | VFunM Integer (forall a. Value -> (Value -> IO a) -> IO a)
   | VVector Dimension (Array Int Value)
   | VMatrix Dimension Dimension (Array (Int,Int) Value)
   | forall i . (Boolean i,PP i) => VSet (SetI i) (FiniteSet i)
   | VTuple [Value]
   | VCon Int String String [Value]
   | VDomain [Dimension]
   | VNS NonStandard
  deriving Typeable

-----------------------------------------------------
-- Something acts like a Boolean if it supports these
-- operations

class Show b => Boolean b where
  true :: b
  false :: b
  isTrue :: b -> Bool
  isFalse :: b -> Bool
  conj:: b -> b -> b     -- conjunction
  disj:: b -> b -> b     -- disjunction
  neg:: b -> b           -- negation
  imply:: b -> b -> b    -- implication
  
-----------------------------------------------------
-- Helper functions about the representaion of Bools

trueV = (VCon 0 "Bool" "True" [])
falseV = (VCon 0 "Bool" "False" [])

fromBool True = trueV
fromBool False = falseV

valTrue (VCon 0 "Bool" "True" []) = True
valTrue (VBase (LCon "Bool" "True")) = True
valTrue _ = False

valFalse (VCon 0 "Bool" "False" []) = True
valFalse (VBase (LCon "Bool" "False")) = True
valFalse _ = False

toPropInt:: Value -> Prop Int
toPropInt (VNS (NSProp x)) = x
toPropInt v | valTrue v = TruthP
toPropInt v | valFalse v = AbsurdP
toPropInt x = error ("Not a proposition in toPropInt: "++show x)

initEnumerations "True" = Just "Bool"
initEnumerations "False" = Just "Bool"
initEnumerations "Nothing" = Just "Maybe" 
initEnumerations _ = Nothing

-------------------------------------------------------
-- Operations on Literals

instance Simple Literal where
  simple (LInt n) = simple n
  simple (LDouble d) = show d
  simple (LChar c) = simple c
  simple (LString s) = simple s
  simple LUnit = "()"
  simple (LCon tnm c) = c


kIndex:: [Dimension] -> Int -> [Int]     
kIndex dims m = help m (reverse (fmap (\(Dim n base xs)->n) dims)) []
  where help 0 [] ans = ans
        help 0 (d:ds) ans = help 0 ds (0:ans)
        help n (d:ds) ans = help (n `div` d) ds ((n `mod` d):ans)  
        help n [] ans = error ("The input "++show m++" is too large for the dimensions "++ dimNameL dims)

  
litToIndex :: Dimension -> Literal -> Int                         
litToIndex (Dim n base xs) a =
   case elemIndex a xs of
     Just n -> n
     Nothing -> error ("The element: "++show a++" can't be found in the index set "++show xs)

indexToLit :: Dimension -> Int -> Literal
indexToLit (d@(Dim n base xs)) a 
   | (a < 0) || (a >= length xs) 
   = error ("Bad indexToLit "++show d++" "++show a)
indexToLit (Dim n base xs) a = xs !! a

dimTypeName (Dim n base xs) = show base

litToBase (LInt n) = Int
litToBase (LDouble n) = Double
litToBase (LChar c) = Char
litToBase (LString s) = String
litToBase (LCon "Bool" "True") = Bool
litToBase (LCon "Bool" "False") = Bool
litToBase (LCon tn n) = Enum tn
litToBase l = error ("Literal does not have base type: "++show l)

typeNameLit (LInt n) = "Int"
typeNameLit (LDouble n) = "Double"
typeNameLit (LChar c) = "Char"
typeNameLit (LString s) = "String"
typeNameLit (LCon tnm name) = tnm
typeNameLit LUnit = "()"

------------ Printing and Formatting things ------------

-- lists of strings

-- Base 
instance Show Base where
  show Int = "Int"
  show Char = "Char"
  show Double = "Double"
  show String = "String"
  show Bool = "Bool"
  show (Enum s) = s
  show (Tuple xs) = plistf show "#(" xs "," ")"

-- Literal  
instance Show Literal where
  show (LInt n) = show n
  show (LDouble n) = show n
  show (LChar c) = show c
  show (LString s) = ['"']++s++['"']
  show (LCon tnm name) = name
  show LUnit = "()"
  

-- Dimensions
dimName (Dim n base ls) =  show base++"#"++show n
dimNameAll (Dim n base (l:ls)) = show base++"#"++show(l:ls)
dimSize (Dim n base ls) = n
dimElem (Dim n base ls) = ls

dimNameL ds = plistf dimName "(" ds "," ")"

instance Show Dimension where
  show x = dimNameAll x

instance Simple Value where
  simple (VBase x) = simple x
  simple x = show x

splat (VBase x) = "(VBase "++show x++")"
splat (VCon n typ name xs) = plistf splat ("VCon "++name) xs " " ")"
splat x = showVal x

-- Values 
instance Show Value where
 show x = showVal x
 

showVal (VBase x) = show x
showVal (VFun x) = "<Fun>"
showVal (VFunM n x) = "<FunM"++show n++">"
showVal (u@(VVector d v)) = boxVector u
showVal (u@(VMatrix d1 d2 v)) = boxMatrix u
showVal (VSet BoolI set) = show set
showVal (VSet PropI set) = show set  
showVal (VTuple []) = "()"
showVal (VTuple [x]) = show x
showVal (VTuple xs) = "("++f xs
  where f [x] = show x++")"
        f (x:xs) = show x++ ","++f xs  
showVal (v@(VCon n typ _ _)) | Just vs <- isListV v = plistf show "[" vs "," "]"
showVal (v@(VCon n typ _ _)) | Just str <- isStringV v = if null str then "[]" else str
showVal (VCon n typ name []) = name
showVal (VCon n typ name xs) = "("++name++" "++f xs
  where f [x] = show x++")"
        f (x:xs) = show x++ " "++f xs  
showVal (VDomain [d]) = dimName d         
showVal (VDomain ds) = plistf dimName "#(" ds "," ")"
showVal (VNS c) = showNS c
  
-------------------------------------------------
-- Types of values


typeNameVal :: Value -> String
typeNameVal (VBase x)   = typeNameLit x
typeNameVal (VFun g)    = "<fun>"
typeNameVal (VFunM n g)    = "<fun>"
typeNameVal (VVector d v) = "Array "++domainRoot d++" "++typeNameVal (v ! 0)
typeNameVal (VMatrix d1 d2 v) = 
   "Array ("++domainRoot d1++","++domainRoot d2++") "++typeNameVal (v ! (0,0))
typeNameVal (VSet _ (FA ds _)) = plistf domainRoot "Set(" ds "," ")"
typeNameVal (VTuple vs) = plistf typeNameVal "(" vs "," ")"
typeNameVal (v@(VCon _ _ _ _)) | Just vs <- isListV v =
  case vs of
   [] -> "List a"
   (v:vs) -> "List "++typeNameVal v
typeNameVal (VCon n typ c vs) = typ
typeNameVal (VDomain [d]) = "Dim "++ domainRoot d
typeNameVal (VDomain ds) = plistf domainRoot "Dim (" ds "," ")"
typeNameVal (VNS c) = typeNameNS c 
-- typeNameVal v = "??"

domainRoot (Dim n base ls) = show base


instance Eq Literal where
  (LInt n) == (LInt m) = n==m
  (LDouble n) == (LDouble m) = n==m
  (LChar n) == (LChar m) = n==m
  (LString n) == (LString m) = n==m
  (LCon t x) == (LCon s y) = x==y  
  LUnit == LUnit = True
  _ == _ = False
  
                -- Print Bool    -- Layout elements  
ppFA :: PP a => (Doc -> Doc) -> ([Doc] -> a1) -> FiniteSet a -> a1
ppFA g cat (FA [] map) =
  case (DM.toAscList map) of
    [(i,x)] -> cat [text "Scalar ",pp x]
    other -> error ("Badly formed zero dimensional table")
ppFA g cat (FA dims map) = cat (text (dimNameL dims) :(fmap widen (DM.toAscList map)))
  where widen (k,x) = -- (text ("KEY "++show k++show(kIndex dims k))) <+> 
                         toDoc(flatIndexToLits dims k,pp x)
        toDoc (is,str) = text (plistf show "(" is "," ")") <> g str
        flatIndexToLits ds n = (zipWith f ds tuple)
          where tuple = kIndex ds n
                f (Dim n base xs) m = xs !! m
        kIndex:: [Dimension] -> Int -> [Int]     
        kIndex dims m = help m (reverse (fmap (\(Dim n base xs)->n) dims)) []
          where help 0 [] ans = ans
                help 0 (d:ds) ans = help 0 ds (0:ans)
                help n (d:ds) ans = help (n `div` d) ds ((n `mod` d):ans)  
                help n [] ans = error ("The input "++show m++" is too large for the dimensions "++ dimNameL dims)               

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
    where f (dimDoc : docs) = dimDoc $$ PP.braces(PP.fsep docs)
          g x = text "=" <> x         
 
instance Show (FiniteSet Bool) where
  show (x@(FA dims tab)) = render(pp x)

instance Show (FiniteSet (Prop Int)) where
  show (x@(FA dims tab)) = render(pp x)
 
  
--------------------------------------------------------   

class Encoding t where
   to     :: t -> Value 
   from   :: Value -> t
                         
toStr = VBase . LString

fromStr (VBase(LString s)) = s
fromStr (VCon 0 "List" "[]" []) = []
fromStr (VCon 2 "List" ":" [VBase(LChar c),xs]) = c : (fromStr xs)
fromStr v = error ("Non String in fromStr.")

isString (VBase(LString s)) = True
isString (VCon 0 "List" "[]" []) = True
isString (VCon 2 "List" ":" [VBase(LChar c),xs]) = isString xs
isString v = False                               

---------------------------------
-- instances

instance Encoding () where
   to x = VBase LUnit
   from (VBase LUnit) = ()
   from v = error ("Value not unit: "++(show v))

instance Encoding Value where
    to x = x
    from x = x
    
instance Encoding Int where
    to n = VBase(LInt n)
    from (VBase(LInt n)) = n
    from v = error ("Value not an Int: "++(show v))

instance Encoding Double where
    to n = VBase(LDouble n)
    from (VBase(LDouble n)) = n
    from v = error ("Value not a Double: "++(show v))

instance Encoding Char where
    to n = VBase(LChar n)
    from (VBase(LChar n)) = n
    from v = error ("Value not a Char: "++(show v))

instance Encoding Bool where
    to = fromBool
    from x | valTrue x = True
    from x | valFalse x = False
    from v = error ("Value not Bool: "++(show v))

instance Encoding a => Encoding (Maybe a) where
    to Nothing = VCon 0 "Maybe" "Nothing" []
    to (Just x) = VCon 1 "Maybe" "Just" [to x]
    from (VCon 0 "Maybe" "Nothing" []) = Nothing
    from (VCon 1 "Maybe" "Just" [x]) = (Just (from x))
    from v = error ("Value not a Maybe type: "++show v)

instance (Encoding a,Encoding b) => Encoding (Either a b) where
    to (Right x) =  (VCon 1 "Either" "Right" [to x])
    to (Left x) =   (VCon 1 "Either" "Left" [to x])
    from (VCon 1 "Either" "Left" [x]) = Left(from x)
    from (VCon 1 "Either" "Right" [x]) = Right(from x)
    from v = error ("Value not an Either type: "++show v)    

instance (Encoding a,Encoding b) => Encoding (a,b) where
    to (a,b) = VTuple[to a,to b]
    from (VTuple[x,y]) = (from x,from y)
    from v = error ("Value not a pair: "++(show v))

instance (Encoding a,Encoding b,Encoding c) => Encoding (a,b,c) where
    to (a,b,c) = VTuple[to a, to b, to c]
    from (VTuple[x,y,z]) = (from x,from y,from z)
    from v = error ("Value not a triple: "++(show v))

instance (Encoding a,Encoding b,Encoding c,Encoding d) => Encoding (a,b,c,d) where
    to (a,b,c,d) = VTuple[to a, to b, to c, to d]
    from (VTuple[w,x,y,z]) = (from w,from x,from y,from z)
    from v = error ("Value not a quadruple: "++(show v))

------------------------------------------------------------
-- Arrays are implemented as Vectors and Matrices

instance Encoding v => Encoding (Array Int v) where
   to x = VVector (Dim (high-low+1) Int (map LInt [low .. high]))
                  (listArray (0,high-low) (map f elems))
      where (low,high) = bounds x
            elems = assocs x
            f (i,v) = (to v)
   from (VVector d v) = fmap from v
   
instance Encoding v => Encoding (Array (Int,Int) v) where
   to x = VMatrix (Dim (high-low+1) Int (map LInt [low .. high]))
                  (Dim (h-l+1) Int (map LInt [l .. h]))
                  (listArray ((0,0),(high-low,h-l)) (map f elems))
      where ((low,l),(high,h)) = bounds x
            elems = assocs x
            f (i,v) = (to v) 
   from (VMatrix d1 d2 v) = fmap from v            

-- index:: Array t x -> t -> x
indexV = VFun f
  where f :: Value -> Value
        f (VVector d arr) = (VFun (g d arr))
        f (VMatrix d1 d2 arr) = (VFun (h (d1,d2) arr))
        f v = error ("Non array as first arg of 'index'."++show v) 
        g :: Dimension -> Array Int Value -> Value -> Value
        g d arr (VBase lit) = (arr  ! (litToIndex d lit))
        g d arr v = error ("In a call to 'index' on a Vector, illegal index: "++show v)
        h :: (Dimension,Dimension) -> Array (Int,Int) Value -> Value -> Value
        h (d1,d2) arr (VTuple [VBase i,VBase j]) = (arr  ! (litToIndex d1 i,litToIndex d2 j))
        h ds arr v = error ("In a call to 'index' on a Matrix, illegal index: "++show v)                
        
-- array:: Domain d -> [t] -> Array d t
arrayV = VFun f
  where f (VDomain [d]) = (VFun (g d))
        f (VDomain [d1,d2]) = (VFun (h (d1,d2)))
        f (v@(VDomain ds)) = error ("'array' is only implemented for 1-D and 2-D arrays. This dimension is too wide: "++show v)
        f v = error ("Non dimension as first arg to 'array'."++show v)
        g d v | Just vs <- isListV v = 
           if length vs == (dimSize d)
              then (VVector d (listArray (0,dimSize d -1) vs))
              else error ("Vector initialization list for 'array' does not have correct number of elements\n"++
                          "for dimension "++show(VDomain [d])++"\n"++show vs)              
        g d v = error ("Non list as 2nd arg of 'array'."++show v)  
        h (d1,d2) v | Just vs <- isListV v = 
          if length vs == (dimSize d1)*(dimSize d2)
             then (VMatrix d1 d2 (listArray ((0,0),(dimSize d1 -1,dimSize d2 -1)) vs))
             else error ("Matrix initialization list for 'array' does not have correct number of elements\n"++
                         "for dimensions "++show(VDomain [d1,d2])++"\n"++show vs)
        h d v = error ("Non list as 2nd arg of 'array'."++show v)          

-- arrayDim:: Array d t -> Domain d        
arrayDimV :: Value
arrayDimV = VFun g
  where g :: Value -> Value
        g (VVector d arr) = VDomain [d]
        g (VMatrix d1 d2 arr) = VDomain[d1,d2]
        g v = error ("Non array as arg of 'arrayDim'."++show v)   


setDimV :: Value
setDimV = VFun g
  where g :: Value -> Value
        g (VSet _ (FA ds _)) = VDomain ds
        g v = error ("Non Set as arg of 'setDim'."++show v)   


-- elem :: Domain d -> d -> Bool
elemV :: Value
elemV = VFun f
  where f (VDomain ds) = VFun(g ds)
        f (v@(VTuple _)) = error ("First arg of 'elem' is not a dimension: "++show v++"\nIt is a tuple. Perhaps you forgot the #?")
        f v = error ("First arg of 'elem' is not a dimension: "++show v)
        g [Dim n base xs] (VBase x) = to(elem x xs)
        g ds (VTuple vs) | length ds==length vs = to(and(zipWith f ds vs))
          where f (Dim n base xs) (VBase x) = elem x xs
        g ds v = error ("\nSecond argument to 'elem'\n  "++show v++
                        "\ndoes not match up with the first argument\n   "++show (VDomain ds))
                                                
-- dimensions :: Domain d -> tmap {x.List x} d
enumV :: Value
enumV= VFun g
  where g :: Value -> Value
        g (VDomain ds) =
          let f (Dim size base ls) = to(map VBase ls)
              h [d] = f d
              h ds = VTuple(map f ds)
          in h ds
        g v = error ("Non vector as arg of 'vectorD'."++show v)   

-- A set of tuples to an Array
setToArrayV :: Value
setToArrayV = VFunM 104 f
  where f :: Value -> (Value -> a) -> a
        f (VSet BoolI (set@(FA [d1,d2] _))) k = k(VVector d1 undefined)
           where pairs = zap set
        f (VSet BoolI (set@(FA [d1,d2,d3] _))) k = k(VMatrix d1 d2 arr)
           where pairs = zap set
                 g ([(i,_),(j,_),(_,v)],_) = ((i,j),VBase v)
                 arr = array ((0,0),(dimSize d1 -1, dimSize d2 -1)) (map g pairs)
                 
        f v k = error ("Non finite set as argument to 'toVector'\n"++show v)

   
--------------------------------------------------
-- Encoding Lists

instance Encoding a => Encoding [a] where
    to x = listVal (map to x)
    from (VBase(LString cs)) = map (from . VBase . LChar) cs
    from x | Just vs <- isListV x = map from vs
           | True = error("Value not a list: "++show x)
                                 
listVal vs = foldr cons nil vs
  where cons (VBase (LChar c)) (VCon 0 "List" "[]" []) = VBase(LString [c])
        cons (VBase (LChar c)) (VBase(LString cs)) = VBase(LString (c:cs))
        cons x xs = VCon 2 "List" ":" [x,xs]
        nil = VCon 0 "List" "[]" []  

nilV = VCon 0 "List" "[]" [] 
consV x xs = VCon 2 "List" ":" [x,xs]
        
isListV (VCon 0 "List" "[]" []) = return []
isListV (VCon 2 "List" ":" [x,xs]) =
  do { ys <- isListV xs; return(x:ys)}
isListV (VBase (LString cs)) = return(map (VBase . LChar) cs)  
isListV _ = Nothing  

isIntList (VCon 0 "List" "[]" []) = return []
isIntList (VCon 2 "List" ":" [VBase (LInt x),xs]) =
  do { ys <- isIntList xs; return(x:ys)}
isIntListL _ = Nothing 

isStringV (VBase (LString s)) = return s
isStringV (VCon 0 "List" "[]" []) = return []
isStringV (VCon 2 "List" ":" [VBase(LChar c),xs]) =
  do { ys <- isStringV xs; return(c:ys)}
isStringV _ = Nothing 

-----------------------------------------
-- Functions

instance (Encoding a,Encoding b) => Encoding(a -> b) where
    -- works on first order functions only
    to f = VFun(to . f . from)
    from (VFun f) = error ("'from' for functions not defined.")
    from (VFunM n f) = error ("'from' for monadic functions not defined.")
  --  from (VFunC n f) = error ("'from' for monadic2 functions not defined.")
    from x = error("Value not a function: "++show x)

-- Creating values encoding functions in the IO monad
liftIO1 n f = (VFunM n(\ x k -> do { ans <- f(from x); k(to ans)}))
liftIO2 n f = (VFun(\ x -> liftIO1 n (f (from x))))
    
liftIII :: (Int -> Int -> Int) -> Value
liftIII f = to f


liftIII3 :: String -> (Int -> Int -> Int) -> 
                     (ExpY -> ExpY -> ExpY) -> 
                     (MExp Int -> MExp Int -> MExp Int) -> Value
liftIII3 s f g mf = VFunM 0 f2
  where f2:: Value -> (Value -> IO a) -> IO a
        f2 (VBase(LInt n)) k = k (VFunM 0 (f3 n))
        f2 (VNS (NSYices e)) k = k (VFunM 0 (f4 e))
        f2 (VNS (NSMath x)) k = k (VFunM 0 (f5 x))
        f2 v k =  error ("\nA. Non Int in first argument to "++s++"\n  "++show v)
        
        f3:: Int -> Value -> (Value -> IO a) -> IO a
        f3 n (VBase (LInt m)) k  = k(VBase(LInt (f n m)))
        f3 n (VNS (NSYices e)) k = k(VNS(NSYices (g (LitI n) e)))
        f3 n (VNS (NSMath e)) k  = k(VNS(NSMath (mf (Term [] n) e)))
        f3 n v k =  error ("\nA. Non Int in first argument to "++s++"\n  "++show v)                              

        f4:: ExpY -> Value -> (Value -> IO a) -> IO a                              
        f4 e (VBase(LInt n)) k = k(VNS(NSYices (g e (LitI n))))
        f4 e (VNS(NSYices y)) k = k(VNS(NSYices(g e y)))
        f4 e v k =  error ("\nB. Non Int in first argument to "++s++"\n  "++show v)

        f5:: MExp Int -> Value -> (Value -> IO a) -> IO a 
        f5 x (VNS (NSMath y)) k = k(VNS(NSMath (mf x y)))
        f5 x (VBase(LInt n)) k = k(VNS(NSMath (mf x (Term [] n))))
        f5 x v k =  error ("\nC. Non Int in first argument to "++s++"\n  "++show v)        

-- liftTree t f = treeToValue(do { m <- t; Pause(f m)})                              


liftIIB :: (Int -> Int -> Bool) -> Value
liftIIB f = to f
        
liftIIB3 :: String -> (Int -> Int -> Bool) -> 
                      (ExpY -> ExpY -> ExpY) -> 
                      (MExp Int -> MExp Int -> [Rel Int]) ->
                      Value
liftIIB3 s f g mf = VFunM 0 f2
  where f2:: Value -> (Value -> IO a) -> IO a
        f2 (VBase(LInt n)) k = k (VFunM 0 (f3 n))
        f2 (VNS (NSYices e)) k = k (VFunM 0 (f4 e))
        f2 (VNS (NSMath e)) k = k (VFunM 0 (f5 e))
        f2 v k =  error ("\nD. Non Int in first argument to "++s++"\n  "++show v)

        f3:: Int -> Value -> (Value -> IO a) -> IO a
        f3 n (VBase (LInt m)) k = k( to (f n m) )
        f3 n (VNS (NSYices e)) k = k(VNS(NSYices (g (LitI n) e)))
        f3 n (VNS (NSMath e)) k = k(VNS(NSRel (mf (Term [] n) e)))                              

        f4:: ExpY -> Value -> (Value -> IO a) -> IO a                              
        f4 e (VBase(LInt n)) k = k(VNS(NSYices (g e (LitI n))))
        f4 e (VNS(NSYices y)) k = k(VNS(NSYices(g e y)))
        f4 e v k =  error ("\nE. Non Int in first argument to "++s++"\n  "++show v) 

        f5:: MExp Int  -> Value -> (Value -> IO a) -> IO a                              
        f5 x (VNS (NSMath y)) k = k(VNS(NSRel (mf x y)))  
        f5 x (VBase(LInt n))  k = k(VNS(NSRel (mf x (Term [] n))))  
        f5 e v k =  error ("\nF. Non Int in first argument to "++s++"\n  "++show v)

unsup s x y = error ("Overloading of "++s++" is not supported")

prims = [("+",liftIII3 "+" (+) (:+:) plusM)
        ,("-",liftIII3 "-" (-) (:-:) minusM)
        ,("*",liftIII3 "*" (*) (:*:) timesM)
        ,("/",liftIII3 "div" (div) (DIV) (unsup "div") )
        ,("div",liftIII3 "div" (div) (DIV) (unsup "div"))        
        ,("mod",liftIII3 "mod" (mod) (MOD) (unsup "mod"))        
        ,("&&",andV)
        ,("<=>",equalV)
        ,("||",orV)
        ,("not",notV)
        ,("=>",impliesV)
        ,("<",liftIIB3 "<" (<) (:<) (ltM))
        ,("<=",liftIIB3 "<="  (<=) (:<=)(lteqM))
        ,(">=",liftIIB3 ">=" (>=) (:>=) (gteqM))
        ,(">",liftIIB3 ">" (>) (:>) (gtM))
        ,("==",liftIIB3 "==" (==) (:=) (eqM))
        ,("/=",liftIIB3 "/=" (/=) (:/=) (unsup "/="))

        ,("pnG",pnGV)
        ,("pnG2",pnG2V)        
        ,("show",showV)
        ,("setToList",tolistV)
        ,("setToArray",setToArrayV)
        ,("arrayDim",arrayDimV)
        ,("setDim",setDimV)
        ,("index",indexV)
        ,(".",indexV)
        ,("array",arrayV)
        ,("enum",enumV)
        ,("elem",elemV)
        ]

showV = VFun f where f v = to(show v)


equalV = VFun f1
  where f1 x | valTrue x = VFun (f2 True)
        f1 x | valFalse x = VFun (f2 False)
        f1 (VNS (NSProp x)) = VFun (f3 x)
        f1 (VNS (NSYices e)) = VFun (f4 e)
        f1 x = error ("Bad first arg to '<=>': "++show x)
        
        f2 True x | valTrue x = to True
        f2 True x | valFalse x = to False
        f2 False x | valFalse x = to True
        f2 False x | valTrue x = to False
        f2 True  b = f3 TruthP b
        f2 False b = f3 AbsurdP b
        
        f3 x y | valTrue y = (VNS (NSProp (iff x TruthP)))
        f3 x y | valFalse y = (VNS (NSProp (iff x AbsurdP)))        
        f3 x (VNS (NSProp y)) = (VNS (NSProp (iff x y)))
        f3 _ x = error ("Bad second arg to '<=>': "++show x)
        
        f4 e y | valTrue y = (VNS (NSYices e))
        f4 e y | valFalse y = (VNS (NSYices (NOT e)))
        f4 e (VNS (NSYices x)) = VNS(NSYices (andY[e :=> x, x :=> e]))
        f4 e x = error ("Bad first second to '<=>': "++show x)           


andV = VFun f1
  where f1 x | valTrue x = VFun (f2 True)
        f1 x | valFalse x = VFun (f2 False)
        f1 (VNS (NSProp x)) = VFun (f3 x)
        f1 (VNS (NSYices e)) = VFun (f4 e)
        f1 (VNS (NSRel xs)) = VFun (f5 xs)
        f1 x = error ("Bad first arg to '&&': "++show x)
        
        f2 True v = v
        f2 False v = to False

        f3 TruthP v = v
        f3 AbsurdP v = (VNS (NSProp AbsurdP))
        f3 x (VNS (NSProp y)) = (VNS (NSProp (andB x y)))
        f3 x y | valTrue y = (VNS (NSProp (andB x TruthP)))
        f3 x y | valFalse y = (VNS (NSProp (andB x AbsurdP)))        
        f3 _ x = error ("Bad second arg to '&&': "++show x)

        f4 e y | valTrue y = VNS(NSYices e)
        f4 e y | valFalse y = falseV
        f4 e (VNS (NSYices x)) = VNS(NSYices (andY [e,x]))
        f4 e x = error ("Bad first second to '&&': "++show x)

        f5 xs (VNS (NSRel ys)) = VNS(NSRel (andM xs ys))
        f5 xs v | valTrue v = VNS(NSRel xs)
        f5 xs v | valFalse v = VNS(NSRel [UNSAT])
        f5 e x = error ("Bad first second to '&&': "++show x)        
        
orV = VFun f1
  where f1 x | valTrue x = VFun (f2 True)
        f1 x | valFalse x = VFun (f2 False)
        f1 (VNS (NSProp x)) = VFun (f3 x)
        f1 (VNS (NSYices e)) = VFun (f4 e)
        f1 x = error ("Bad first arg to '||': "++show x)

        f2 True v = to True
        f2 False v = v

        f3 TruthP v = (VNS (NSProp TruthP))
        f3 AbsurdP v = v
        f3 x (VNS (NSProp y)) = (VNS (NSProp (orB x y)))
        f3 x y | valTrue y = (VNS (NSProp (orB x TruthP)))
        f3 x y | valFalse y = (VNS (NSProp (orB x AbsurdP)))        
        f3 _ x = error ("Bad second arg to '||': "++show x)  

        f4 e x | valTrue x = trueV
        f4 e x | valFalse x = (VNS (NSYices e))
        f4 e (VNS (NSYices x)) = VNS(NSYices (orY [e,x]))
        f4 e x = error ("Bad first second to '||': "++show x)        

        
impliesV = VFun f1
  where f1 x | valTrue x = VFun (f2 True)
        f1 x | valFalse x = VFun (f2 False)
        f1 (VNS (NSProp x)) = VFun (f3 x)
        f1 (VNS (NSYices e)) = VFun (f4 e)
        f1 x = error ("Bad first arg to '||': "++show x)

        f2 True v = v
        f2 False v = to True

        f3 TruthP v = v
        f3 AbsurdP v = (VNS (NSProp TruthP))
        f3 x (VNS (NSProp y)) = (VNS (NSProp (implyB x y)))
        f3 x (VNS (NSYices e)) = error ("No lifting from Prop to Yices Yet")
        f3 x y | valTrue y = (VNS (NSProp (implyB x TruthP)))
        f3 x y | valFalse y = (VNS (NSProp (implyB x AbsurdP)))                
        f3 _ x = error ("Bad second arg to '||': "++show x)

        f4 e y | valTrue y = (VNS (NSYices (e :=> LitB True)))
        f4 e y | valFalse y = (VNS (NSYices (e :=> LitB False)))
        f4 e (VNS (NSYices x)) = (VNS (NSYices (e :=> x)))
        f4 e x = error ("Bad first second to '||': "++show x)        
        


notV = VFun notF
notF x | valTrue x = falseV
notF x | valFalse x = trueV
notF (VNS (NSProp x)) = (VNS (NSProp (notB x)))  
notF (VNS (NSYices e)) = VNS (NSYices (NOT e))
notF v = error ("Bad second to 'not': "++show v)   

pnG2V = VFunM 100 pnG2F
 
pnG2F:: Value -> (Value -> IO a) -> IO a
pnG2F (VSet BoolI x) c = c(VFunM 101 (pnG2Help x))
pnG2Help:: FiniteSet Bool -> Value -> (Value -> IO a) -> IO a
pnG2Help x (VSet BoolI y) c = 
    pnG (x,y) >> 
    putStrLn ("\n1 = "++show x++"\n2 = "++show y++"\nNodes "++show (nodes (x,y))++"\nEdges "++show(edges (x,y)))  >> 
    c (to ())

pnGV = VFunM 100 pnGF
 
pnGF (VSet PropI x) c = pnG x >> c (to ())
pnGF (VSet BoolI x) c = pnG x >> c (to ())
pnGF v c = error ("Can't pnG:\n  "++show v)

tolistV :: Value
tolistV = VFunM 103 f
  where f:: Value -> (Value -> a) -> a
        f (VSet BoolI y) k = k (to(map g (zap y)))
        f v k = error ("Non finite set as argument to 'tolist'\n"++show v)
        g :: ([(Int, Literal)], Bool) -> Value
        g (pairs,bool) = VTuple (map (\ (i,lit) -> VBase lit) pairs)
        
   
------------------------------------------------------------
instance Simple a => Graph (FiniteSet a) where
  nodes (x@(FA [d1,d2] set)) | d1==d2 = concat(map h (edges x))
    where get (Dim n b xs) i = (i,simple(xs !! i),None,Circle)
          h (x,y,str,col) = [get d1 x,get d2 y]
    
  nodes (FA ds set) = error "Binary relations only"
  edges (FA [d1,d2] set) | d1==d2 = map f pairs
    where pairs = DM.toList set
          f (i,t) = g (kIndex [d1,d2] i,simple t,None)
          g ([x,y],str,col) = (x,y,str,col)
  edges (FA ds set) = error "Binary relations only"

zap (FA ds set) = map f (DM.toList set)
  where f (i,x) = (zipWith get ds (kIndex ds i),x)
        get (Dim n b xs) i = (i,xs !! i)
  
instance Simple a => Graph (FiniteSet Bool,FiniteSet a) where
  nodes (x@(FA [d1,d2] set1),y@(FA [c1,c2] set2)) = map f es
    where getNodes ([(i,labelI),(j,labelJ)],v) = [(i,labelI),(j,labelJ)]
          getColor ([(i,node),(j,color)],v) = (i,color)
          es = nub (concat (map getNodes (zap x)))
          cs = map getColor (zap y)
          f (i,label) = case lookup i cs of
                          Just c -> (i,simple label,str2Color(simple c),Circle)      
  edges (x@(FA [d1,d2] set1),y@(FA [c1,c2] set2)) = edges x
    

instance Simple (Prop Int) where
  simple TruthP = "T"
  simple AbsurdP = "F"
  simple (LetterP x) = "p"++show x
  simple (AndP x y) = "And"
  simple (OrP x y) = "Or"
  simple (ImpliesP x y ) = "Implies"
  simple (NotP _) = "Not"
 
----------------------------------------------------------
-- Mucking with different 'a' instances of (FiniteSet a)                            

data SetI t where
  BoolI :: SetI Bool
  PropI :: SetI (Prop Int)

{-  See liftFiniteSet
toProp :: FiniteSet Bool -> FiniteSet (Prop Int)
toProp (FA ds table) = FA ds (DM.map f table)
  where f True = TruthP
        f False = AbsurdP
-}        
        
-----------------------------------
instance Eq Dimension where
  (Dim n b1 xs) == (Dim m b2 ys) = n==m && b1==b2 && xs==ys
                                                                                                                            
instance Eq a => Eq(FiniteSet a) where
  (FA ds1 m1) == (FA ds2 m2) =  DM.size m1 == DM.size m2 && ds1==ds2 && m1==m2

------------------------------------------------------

data NonStandard 
  = NSProp (Prop Int)
  | NSYices ExpY
  | NSMath (MExp Int)
  | NSRel [Rel Int]

varsV (VNS(NSProp p)) = vars p []
varsV x = []

propFromValue (VNS(NSProp p)) = p  
propFromValue v = TruthP

showNS (NSProp t) = show t
showNS (NSYices x) = ("`" ++ show x) 
showNS (NSMath x) = "`"++show x
showNS (NSRel x) = plistf show "`" x " &&\n " ""

typeNameNS (NSProp t) = "SAT"
typeNameNS (NSYices t) = "SMT"
typeNameNS (NSMath t) = "MPnum"
typeNameNS (NSRel t) = "MPbool"

applyNS applyv (VNS(NSProp t)) arg k = error ("Should not happen prop Fun2")
applyNS applyV f   (VNS(NSProp c)) k = error ("Should not happen prop Arg2")

reify:: Value -> ExpY
reify (VNS (NSYices e)) = e
reify x | valTrue x = LitB True
reify x | valFalse x = LitB False
reify (VBase (LInt n)) = LitI n
reify (VBase (LDouble n)) = LitR(approxRational n 0.0001)
reify (VTuple vs) = MKTUP(map reify vs)
reify v = error ("reify not defined for: "++show v)
  

---------------------------------------------------------------
-- Searching for answers, by creating a lazy list

data Lazy a = NoSol | SomeSol a (IO (Lazy a))

lazyMap f x = do { y <- x; help y }
  where help NoSol = return NoSol
        help (SomeSol v xs) = return(SomeSol (f v) (lazyMap f xs))

bfsV
  :: DM.Map Integer Bool
     -> Value
     -> IO(Lazy Value)
     -> (Value -> Value)
     -> (Value -> IO(Lazy Value) -> IO (Lazy Value))  
     -> IO (Lazy Value)
bfsV tab (VTuple vs) zs f k = help vs [] zs
  where help (v:vs) us zs = bfsV tab v zs id (\ u zs2 -> help vs (u:us) zs2)
        help [] us zs = k (f(VTuple(reverse us))) zs
bfsV tab (VCon n tn c vs) zs f k = help vs [] zs
  where help (v:vs) us zs = bfsV tab v zs id (\ u zs2 -> help vs (u:us) zs2)
        help [] us zs = k (f(VCon n tn c (reverse us))) zs
bfsV tab v zs f k = k (f v) zs

chooseV:: Value -> IO [Value]
chooseV v = 
  do { x <- bfsV DM.empty v (return NoSol) id (\ x xs -> return(SomeSol x xs))
     ; interactL x }

showTab t = concat(map ff (DM.toList t))
  where ff (n,True) = (show n)++"+"
        ff (n,False) = (show n)++ "-"
        
zzz n [] = ""
zzz n ((x,t):xs) = replicate n ' '++show x++" "++showTab t++"\n"++zzz (n+2) xs

interactL NoSol = putStrLn ("No more solutions") >> return []
interactL (SomeSol v c) = 
  do { s <- readLine (show v++"\nmore? (<Enter> for yes)")
     ; case s of
        "" -> do { x <- c; vs <- interactL x; return(v:vs) }
        other -> putStrLn ("User halt") >> return [] }
       
i :: Int -> Value
i n = VBase(LInt n)

cons x xs = VCon 2 "List" ":" [x,xs]

----------------------------------------------------------------------------
firstMinisat:: ([Int] -> IO a) -> FilePath -> FilePath -> [[Prop Int]] -> IO a
firstMinisat action cnfName solName clauses =
  do { ans <- solveClauses cnfName solName clauses
     ; case ans of
         Nothing -> error ("Minisat fails, no solution")
         Just sol -> action sol }

{-
lazyMinisat action cnfName solName clauses = 
        do { i <- nextinteger
           ; ans <- solveClauses cnfName solName clauses
           ; case ans of
               Nothing -> return(Fail "Minisat fails")
               Just sol -> do { let newcls = negatedCl sol : clauses
                                    first = return(Exactly(action sol))
                              ; next <- thunkM (lazyMinisat action cnfName solName newcls)
                              ; return(Choice i (Pause first) (Pause next)) }} 
-}


chooseMinisat:: Show a => ([Int] -> IO(a,b)) -> FilePath -> FilePath -> [[Prop Int]] -> IO b
chooseMinisat action cnfName solName clauses = 
        do { ans <- solveClauses cnfName solName clauses
           ; case ans of
               Nothing -> error ("Minisat fails to find an answer")
               Just sol -> do { let newcls = negatedCl sol : clauses
                              ; (next,ans) <- (action sol)
                              ; x <- readLine ("Solution\n"++show next++"Keep this one? (<enter> = no, anything else is yes)")
                              ; case x of
                                 [] -> chooseMinisat action cnfName solName newcls
                                 _ -> return ans} }
               
  
-- liftToTree :: Monad m => m a -> Tree m b a        
-- liftToTree x = Pause(do { a <- x; return(Exactly a)})   

thunkM x = 
  do { ref <- newIORef (Left x)
     ; let get = do { x <- readIORef ref; overwrite x}
           overwrite (Right x) = return x
           overwrite (Left c) =  
             do { x <- c      
                ; writeIORef ref (Right x)            
                ; return x }
     ; return get }

---------------------------------------------------------
-- For printing 2-D arrays in boxes

pad n x xs | length xs > n = take n xs                     
pad n x xs = xs ++ replicate (n - length xs) x

maxL :: [[a]] -> Int
maxL [] = 0
maxL xs = maximum(map length xs)
  
punc:: a -> [a] -> [a]
punc x [] = []
punc x [y] = [y,x]
punc x (y:ys) = y:x:punc x ys
               
boxManyLines (side,top,cross,clwidths) x1 = (x9,colwidths,rowdepths)  where
        bar x = side++x
        barup x1 = (map (map (map bar)) x1)
        -- need non truncating pad here
        pad :: Int -> a -> [a] -> [a]
        pad n x xs = xs ++ replicate (n - length xs) x
        rowdepths = map maxL x1
        maxlen = maximum (map maxL x1)
        lengths = (map (map maxL) x2)
        colwidths = zipWith max (map maximum (transpose lengths)) clwidths
        line4 = plistf (\ n -> pad n top "") cross colwidths cross (cross)      
        x2 = map (pad maxlen [""]) x1
        x3 = zipWith (\ w xs -> map (pad w "") xs) rowdepths x2
        x4 = map f x3
               where f zs = zipWith g colwidths zs
                     g n ss = map (\ s -> pad n ' ' s) ss
        x5 = map transpose (barup x4)
        x6 = map (map concat) x5
        x7 = map (map (\ x -> x++"|")) x6
        x8 = punc [line4] x7
        x9 = line4 : (concat x8)


go = putStrLn (unlines zs) where
  (zs,cs,rs) = boxManyLines ("|",'-',"+",[1,1,1,1,1])
                 (map (map (:[])) [["1","2","3","4","5"],["2","3","4","5","6"]])

boxLinesWithNL :: ([Char], Char, String, [Int])
               -> [[String]] -> ([String], [Int], [Int])        
boxLinesWithNL (side,top,cross,clwidths) x = 
   boxManyLines (side,top,cross,clwidths) (map (map lines) x) 
   
   
boxWithLabels:: [String] -> [String] -> [[String]] -> String   
boxWithLabels rowlabels collabels x = (unlines (line : zipWith (++) rowStr lines))
  where (lines,colwidths,rowdepths) = boxLinesWithNL ("|",'-',"+",clwidths) x
        f (w,s) = pad w ' ' s
        line = sep ++ plistf f " " (zip colwidths collabels) " " ""
        clwidths = map length collabels
        rowLabWidth = maxL rowlabels
        sep = pad rowLabWidth ' ' " "
        evenlabels = map (pad rowLabWidth ' ') rowlabels
        rowStr = sep : (punc2 sep evenlabels rowdepths) ++ [sep]
        punc2 sep [] [] = []
        punc2 sep [x] [c] = x : replicate (c-1)  (pad (length x)' ' "") ++[sep]
        punc2 sep (x:xs) (c:cs) = 
            x :  replicate (c-1)  (pad (length x)' ' "")  ++ (sep : punc2 sep xs cs)

boxMatrix (VMatrix (Dim n _ rows) (Dim m _ cols) v) = "\n"++
            boxWithLabels (map show rows) (map show cols) vs
   where split tags [] = []
         split (t:ts) xs = (take m xs) : (split ts (drop m xs))
         vs = map (map show) (split rows (elems v))
         
arr = to (array ((1,0::Int),(2,4::Int)) [((i,j),i+j) | i <- [1..(2::Int)],j <-[0..4]])
vec = VVector (Dim 5 Int (map LInt [0..4])) (array (0,4) (zip [0..4] (map to "abcde")))

boxVector (VVector (Dim n _ is) arr) = unlines[" "++concat ls,line,'|':concat es,line]
  where vs = elems arr
        labels :: [String]
        labels = map show is
        strs:: [String]
        strs = map show vs
        widths:: [Int]
        widths = zipWith (\ l s ->max (length l) (length s)) labels strs
        g :: String -> Int -> String
        g s w = pad w ' ' s
        ls :: [String]
        ls = punc "  " (zipWith g labels widths)
        es :: [String]
        es = punc "| " (zipWith g strs widths)
        line' = concat(punc "-+" (map (\ n -> replicate n '-') widths))
        line = "+"++tail line'
        
----------------------------------------------------------------

dispVal:: Int -> Int -> Int -> [(Integer,Bool)] -> Value -> IO Doc
dispVal max d n zs (VCon arity tn c vs) = 
  do { ds <- mapM (dispVal max d n zs) vs
     ; return(PP.vcat( text c : ds))}
     
dispVal max d n zs v = return(text (show v))


     
