{-# LANGUAGE PatternGuards, GADTs, RankNTypes, 
    DeriveDataTypeable, FlexibleInstances,
    MultiParamTypeClasses, FlexibleContexts,
    ScopedTypeVariables #-}
module Value where

import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ(Doc,text,int,(<>),(<+>),($$),($+$),render)
-- import Monads(readLine)


import Data.IORef(newIORef,readIORef,writeIORef,IORef)
import Data.List(elemIndex,nub,transpose)
import qualified MyMap as DM 
import Data.Array
import Data.Ratio
import Data.Typeable
import Debug.Trace
import Data.List(elemIndex)

import Graphviz(pnG,Graph(..),Color(..),Shape(..),str2Color)
-- import Minisat(solveClauses,negatedCl)
import UniqInteger(nextinteger)

import Auxfuns
import Literal
import Boolean
import Prop
import Bdd
import Dimension
import FiniteSet
import YicesSyntax
import MathProg
import Minisat
import Monads(readLine)

------------------------------------------------------                                                                                                                                
                                                                                                                            
data Value 
   = VBase Literal
   | VFun (Value -> Value)
   | VFunM Integer (forall a. Value -> (Value -> IO a) -> IO a)
   | VVector Dimension (Array Int Value)
   | VMatrix Dimension Dimension (Array (Int,Int) Value)
   | forall i . (BooleanM IO i,PP i) => VSet (SetI i) (FiniteSet i)
   | VTuple [Value]
   | VCon Int String String [Value]
   | VDomain [Dimension]
   | VNS NonStandard
  deriving Typeable
  
data NonStandard 
  = NSProp (Prop Int)
  | NSYices ExpY
  | NSMath (MExp Int)
  | NSRel [Rel Int]  
  | NSBdd (Bdd Int)
  

-----------------------------------------------------

instance Read (Value) where
instance Finite (Value) where
  toLit (VBase x) = [x]
  toLit (VTuple xs) = concat(map toLit xs)
  toLit v = error("Can't turn the value:\n   "++show v++"\ninto a literal.")
  fromLit [x] = VBase x
  fromLit xs = VTuple(map VBase xs)

instance Simple Value where
  simple (VBase x) = simple x
  simple x = show x

-- Values 
instance Show Value where
 show x = showVal x

instance Eq (Value) where
  (VBase x) == (VBase y) = x==y
  (VTuple xs) == (VTuple ys) = xs == ys
  (VCon _ t1 i x) == (VCon _ t2 j y) = t1==t2 && i==j && x==y
  (VFunM i f) == (VFunM j g) = i==j
  (VSet BoolI x) == (VSet BoolI y) = x==y
  (VSet PropI x) == (VSet PropI y) = x==y  
  (VDomain x) == (VDomain y) = x==y
  _ == _ = False
 
setType(VSet x _) = show x
setType x = "Not a set"
 
-----------------------------------------------------
-- Helper functions about the representation of Bools,
-- (nullary and unary) Tuples, Lists of Literals, and Dimensions.

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

valTuple :: [Value ] -> Value 
valTuple [] = VBase LUnit
valTuple [x] = x
valTuple xs = VTuple xs

isDims [] = return []
isDims (VDomain x : ys) =
  do { xs <- isDims ys; return(x:xs)}
isDims _ = Nothing


isLitL (VCon 0 "List" "[]" []) = return []
isLitL (VCon 2 "List" ":" [VBase x,xs]) =
  do { ys <- isLitL xs; return(x:ys)}
isLitL _ = Nothing  

 
unLit (VBase x) = x
unLit v = error ("Non base type:\n   "++show v)


------------ Printing and Formatting things ------------
-- lists of strings

splat (VBase x) = "(VBase "++show x++")"
splat (VCon n typ name xs) = plistf splat ("VCon "++name) xs " " ")"
splat x = showVal x

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
-- Computing types from values

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

--------------------------------------------------------   

class Encoding t where
   to     :: t -> Value 
   from   :: Value -> t
   
class Monad m => EncodingM m t where
   fromM:: Value -> m t
                         
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
    
instance Encoding Literal where
    to x = VBase x
    from (VBase x) = x
    from v = error ("Value is not a Literal\n   "++show v)

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
        g d arr (VBase lit) = (arr  ! (litToIndex "function index 1" d lit))
        g d arr v = error ("In a call to 'index' on a Vector, illegal index: "++show v)
        h :: (Dimension,Dimension) -> Array (Int,Int) Value -> Value -> Value
        h (d1,d2) arr (VTuple [VBase i,VBase j]) = (arr  ! (litToIndex "function index 2" d1 i,litToIndex "function index 3" d2 j))
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
isIntList _ = Nothing 

isStringV (VBase (LString s)) = return s
isStringV (VCon 0 "List" "[]" []) = return []
isStringV (VCon 2 "List" ":" [VBase(LChar c),xs]) =
  do { ys <- isStringV xs; return(c:ys)}
isStringV _ = Nothing 

------------------------------------------------------------
-- Encoding on (Prop Int) and (Bdd Int) and Finite Sets

instance Encoding (Bdd Int) where
  to x = VNS(NSBdd x)
  from (VNS(NSBdd x)) = x
  from v | valTrue v = trueBdd
  from v | valFalse v = falseBdd
  from v = error ("Value not a Bdd: "++show v)
  
instance EncodingM IO (Bdd Int) where
  fromM (VNS(NSBdd x)) = return x
  fromM v | valTrue v = return trueBdd
  fromM v | valFalse v = return falseBdd
  fromM (VNS (NSProp x)) = prop2Bdd x
  fromM v = error ("Value not a Bdd: "++show v)
  
instance Encoding (Prop Int) where
  to x = VNS(NSProp x)
  from (VNS(NSProp x)) = x
  from (VNS(NSBdd x)) = dnfBdd x
  from v | valTrue v = TruthP
  from v | valFalse v = AbsurdP
  from v = error ("Value not a Prop: "++show v)  

toFLProp :: SetI b -> b -> Value
toFLProp BoolI x = to x
toFLProp PropI x = to x
toFLProp BddI x = to x

toFLSet :: SetI b -> FiniteSet b -> Value
toFLSet BoolI x = to x
toFLSet PropI x = to x
toFLSet BddI x = to x

-- fromFLVal :: SetI -> 

instance Monad m => EncodingM m (Prop Int) where
  fromM x = return(from x)
     
instance Encoding (FiniteSet Bool) where
  to f = VSet BoolI f
  from (VSet BoolI f) = f
  from v = error ("Value not a (FiniteSet Bool): "++show v)

instance Monad m => EncodingM m (FiniteSet Bool) where
  fromM x = return(from x)
  
instance Encoding (FiniteSet (Prop Int)) where
  to f = VSet PropI f
  from (VSet PropI f) = f
  from (VSet BoolI f) = boolToProp f
  from v = error ("Value not a (FiniteSet (Prop Int)): "++show v) 

instance Monad m => EncodingM m (FiniteSet (Prop Int)) where
  fromM x = return(from x)
  
instance Encoding (FiniteSet (Bdd Int)) where
  to f = VSet BddI f
  from (VSet BddI f) = f
  from v = error ("Value not a (FiniteSet (Prop Int)): "++show v)     

instance  PP(Bdd Int) where
  pp x = text(stringBdd show x)
  
instance EncodingM IO (FiniteSet (Bdd Int)) where
  fromM (VSet BddI f) = return f
  fromM (VSet BoolI f) = return(boolToBdd f)
  fromM (VSet PropI f) = propToBdd f
  fromM v = error ("Value not a (FiniteSet (Prop Int)): "++show v)   
  
-----------------------------------------
-- Functions on Numeric values

instance (Encoding a,Encoding b) => Encoding(a -> b) where
    -- works on first order functions only
    to f = VFun(to . f . from)
    from (VFun f) = error ("'from' for functions not defined.")
    from (VFunM n f) = error ("'from' for monadic functions not defined.")
    from x = error("Value not a function: "++show x)

-- Creating values encoding functions in the IO monad
liftIO1 n f = (VFunM n(\ x k -> do { ans <- f(from x); k(to ans)}))
liftIO2 n f = (VFun(\ x -> liftIO1 n (f (from x))))
    
liftIII :: (Int -> Int -> Int) -> Value
liftIII f = to f


d2r:: Double -> Rational
d2r x = toRational x
d2Exp x = LitR(d2r x)


liftIII3 :: String -> (Int -> Int -> Int) -> 
                     (ExpY -> ExpY -> ExpY) ->
                     (Double -> Double -> Double) -> 
                     (MExp Int -> MExp Int -> MExp Int) -> Value
liftIII3 s f g h mf = VFunM 0 f2
  where f2:: Value -> (Value -> IO a) -> IO a
        f2 (VBase(LInt n)) k = k (VFunM 0 (f3 n))
        f2 (VBase(LDouble n)) k = k (VFunM 0 (f3b n))
        f2 (VNS (NSYices e)) k = k (VFunM 0 (f4 e))
        f2 (VNS (NSMath x)) k = k (VFunM 0 (f5 x))
        f2 v k =  error ("\nA. Non Number in first argument to "++s++"\n  "++show v)
        
        f3:: Int -> Value -> (Value -> IO a) -> IO a
        f3 n (VBase (LInt m)) k  = k(VBase(LInt (f n m)))
        f3 n (VNS (NSYices e)) k = k(VNS(NSYices (g (LitI n) e)))
        f3 n (VNS (NSMath e)) k  = k(VNS(NSMath (mf (Term [] n) e)))
        f3 n v k =  error ("\nB. Non Int in second argument to "++s++"\n  "++show v)        
        
        f3b:: Double -> Value -> (Value -> IO a) -> IO a
        f3b n (VBase (LDouble m)) k  = k(VBase(LDouble (h n m)))
        f3b n (VNS (NSYices e)) k = k(VNS(NSYices (g (d2Exp n) e)))
        f3b n v k =  error ("\nC. Non Double in second argument to "++s++"\n  "++show v)                              

        f4:: ExpY -> Value -> (Value -> IO a) -> IO a                              
        f4 e (VBase(LInt n)) k = k(VNS(NSYices (g e (LitI n))))
        f4 e (VBase(LDouble n)) k = k(VNS(NSYices (g e (d2Exp n))))
        f4 e (VNS(NSYices y)) k = k(VNS(NSYices(g e y)))
        f4 e v k =  error ("\nD. Non Int in second argument to "++s++"\n  "++show v)

        f5:: MExp Int -> Value -> (Value -> IO a) -> IO a 
        f5 x (VNS (NSMath y)) k = k(VNS(NSMath (mf x y)))
        f5 x (VBase(LInt n)) k = k(VNS(NSMath (mf x (Term [] n))))
        f5 x v k =  error ("\nE. Non Int in second argument to "++s++"\n  "++show v)        


liftIIB :: (Int -> Int -> Bool) -> Value
liftIIB f = to f
        
liftIIB3 :: String -> (Int -> Int -> Bool) -> 
                      (ExpY -> ExpY -> ExpY) -> 
                      (MExp Int -> MExp Int -> [Rel Int]) ->
                      (Double -> Double -> Bool) ->
                      Value
liftIIB3 s f g mf h = VFunM 0 f2
  where f2:: Value -> (Value -> IO a) -> IO a
        f2 (VBase(LInt n)) k = k (VFunM 0 (f3 n))
        f2 (VBase(LDouble n)) k = k (VFunM 0 (f3b n))
        f2 (VBase lit) k | s=="==" = k(VFunM 0 (f6 lit))
        f2 (VNS (NSYices e)) k = k (VFunM 0 (f4 e))
        f2 (VNS (NSMath e)) k = k (VFunM 0 (f5 e))
        f2 v k =  error ("\nA. Non Number in first argument to "++s++"\n  "++show v)

        f3:: Int -> Value -> (Value -> IO a) -> IO a
        f3 n (VBase (LInt m)) k = k( to (f n m) )
        f3 n (VNS (NSYices e)) k = k(VNS(NSYices (g (LitI n) e)))
        f3 n (VNS (NSMath e)) k = k(VNS(NSRel (mf (Term [] n) e)))
        f3 e v k =  error ("\nB. Non Int in second argument to "++s++"\n  "++show v)        

        f3b:: Double -> Value -> (Value -> IO a) -> IO a
        f3b n (VBase (LDouble m)) k = k( to (h n m) )
        f3b n (VNS (NSYices e)) k = k(VNS(NSYices (g (d2Exp n) e))) 
        f3b e v k =  error ("\nC. Non Double in second argument to "++s++"\n  "++show v)
        

        f4:: ExpY -> Value -> (Value -> IO a) -> IO a                              
        f4 e (VBase(LInt n)) k = k(VNS(NSYices (g e (LitI n))))
        f4 e (VNS(NSYices y)) k = k(VNS(NSYices(g e y)))
        f4 e v k =  error ("\nD. Non Int in second argument to "++s++"\n  "++show v) 

        f5:: MExp Int  -> Value -> (Value -> IO a) -> IO a                              
        f5 x (VNS (NSMath y)) k = k(VNS(NSRel (mf x y)))  
        f5 x (VBase(LInt n))  k = k(VNS(NSRel (mf x (Term [] n))))  
        f5 e v k =  error ("\nE. Non Int in second argument to "++s++"\n  "++show v)
        
        f6 :: Literal -> Value -> (Value -> IO a) -> IO a
        f6 (LChar x1) (VBase (LChar x2)) k = k(to(x1 == x2))
        f6 (LString x1) (VBase (LString x2)) k = k(to(x1 == x2))
        f6 (LUnit) (VBase (LUnit)) k = k(to(x1 == x2))
        f6 (LCon t1 x1) (VBase (LCon t2 x2)) k | t1==t2 = k(to(x1 == x2))
        f6 lit v k = error ("Type mismatch in '==',  "++show (VBase lit)++" and "++show v)

unsup s x y = error ("Overloading of "++s++" is not supported")

----------------------------------------------------------
-- Functions on Booleans in all their forms

applyUnary (VFun f) v = return(f v)
applyUnary (VFunM _ f) v = f v return

applyBinary (VFun f) v1 v2 = applyUnary (f v1) v2
applyBinary (VFunM _ f) v1 v2 = f v1 (\ g -> applyUnary g v2)

equalVal v1 v2 = applyBinary equalV v1 v2
equalV = VFun f1 where
        f1 x | valTrue x = VFunM 99 (f2 True)
        f1 x | valFalse x = VFunM 99 (f2 False)
        f1 (VNS (NSProp x)) = VFunM 99 (f3 x)
        f1 (VNS (NSYices e)) = VFun (f4 e)
        f1 (VNS (NSBdd e)) = VFunM 99 (f5 e)
        f1 x = error ("Bad first arg to '<=>': "++show x)
        
        f2 True x k | valTrue x =k(to True)
        f2 True x k | valFalse x = k(to False)
        f2 False x k | valFalse x = k(to True)
        f2 False x k | valTrue x = k(to False)
        f2 True  b k = f3 TruthP b k
        f2 False b k = f3 AbsurdP b k
        
        f3 x y k | valTrue y = k(VNS (NSProp (iff x TruthP)))
        f3 x y k | valFalse y = k(VNS (NSProp (iff x AbsurdP)))        
        f3 x (VNS (NSProp y)) k = k(VNS (NSProp (iff x y)))
        f3 x (y@(VNS (NSBdd _))) k = do { z <- prop2Bdd x; f5 z y k}
        f3 _ x k = error ("Bad second arg to '<=>': "++show x)
        
        f4 e y | valTrue y = (VNS (NSYices e))
        f4 e y | valFalse y = (VNS (NSYices (NOT e)))
        f4 e (VNS (NSYices x)) = VNS(NSYices (andY[e :=> x, x :=> e]))
        f4 e x = error ("Bad first second to '<=>': "++show x)  
        
        f5 x (VNS(NSBdd y)) k = do { z <- equalBdd x y; k(to z)}
        f5 e other k = error ("Bad first second to '<=>': "++show other) 


        
andVal v1 v2 = applyBinary andV v1 v2
andV = VFun f1
  where f1 x | valTrue x = VFun (f2 True)
        f1 x | valFalse x = VFun (f2 False)
        f1 (VNS (NSProp x)) = VFunM 99 (f3 x)
        f1 (VNS (NSYices e)) = VFun (f4 e)
        f1 (VNS (NSRel xs)) = VFun (f5 xs)
        f1 (VNS (NSBdd e)) = VFunM 99 (f6 e)
        f1 x = error ("Bad first arg to '&&': "++show x)
        
        f2 True v = v
        f2 False v = to False

        f3 TruthP v k = k v
        f3 AbsurdP v k = k (VNS (NSProp AbsurdP))
        f3 x (VNS (NSProp y)) k = k(VNS (NSProp (andP x y)))
        f3 x y k | valTrue y = k(VNS (NSProp (andP x TruthP)))
        f3 x y k | valFalse y = k (VNS (NSProp (andP x AbsurdP)))   
        f3 x (y@(VNS (NSBdd _))) k = do { x2 <- prop2Bdd x; f6 x2 y k}
        f3 _ x k = error ("Bad second arg to '&&': "++show x)

        f4 e y | valTrue y = VNS(NSYices e)
        f4 e y | valFalse y = falseV
        f4 e (VNS (NSYices x)) = VNS(NSYices (andY [e,x]))
        f4 e x = error ("Bad first second to '&&': "++show x)

        f5 xs (VNS (NSRel ys)) = VNS(NSRel (andM xs ys))
        f5 xs v | valTrue v = VNS(NSRel xs)
        f5 xs v | valFalse v = VNS(NSRel [UNSAT])
        f5 e x = error ("Bad first second to '&&': "++show x)  
        
        f6 b1 (VNS(NSBdd b2)) k = do { b3 <- andBdd b1 b2; k(to b3)}
       
orVal v1 v2 = applyBinary orV v1 v2        
orV = VFun f1
  where f1 x | valTrue x = VFun (f2 True)
        f1 x | valFalse x = VFun (f2 False)
        f1 (VNS (NSProp x)) = VFunM 99 (f3 x)
        f1 (VNS (NSYices e)) = VFun (f4 e)
        f1 (VNS (NSBdd e)) = VFunM 99 (f6 e)
        f1 x = error ("Bad first arg to '||': "++show x)

        f2 True v = to True
        f2 False v = v

        f3 TruthP v k = k (VNS (NSProp TruthP))
        f3 AbsurdP v k = k v
        f3 x (VNS (NSProp y)) k = k (VNS (NSProp (orP x y)))
        f3 x y k | valTrue y = k (VNS (NSProp (orP x TruthP)))
        f3 x y k | valFalse y = k (VNS (NSProp (orP x AbsurdP)))
        f3 x (y@(VNS (NSBdd _))) k = do { x2 <- prop2Bdd x; f6 x2 y k}
        f3 _ x k = error ("Bad second arg to '||': "++show x)  

        f4 e x | valTrue x = trueV
        f4 e x | valFalse x = (VNS (NSYices e))
        f4 e (VNS (NSYices x)) = VNS(NSYices (orY [e,x]))
        f4 e x = error ("Bad first second to '||': "++show x)  
        
        f6 b1 (VNS(NSBdd b2)) k = do { b3 <- orBdd b1 b2; k(to b3)}        

impliesVal v1 v2 = applyBinary impliesV v1 v2        
impliesV = VFun f1
  where f1 x | valTrue x = VFun (f2 True)
        f1 x | valFalse x = VFun (f2 False)
        f1 (VNS (NSProp x)) = VFunM 99 (f3 x)
        f1 (VNS (NSYices e)) = VFun (f4 e)
        f1 (VNS (NSBdd e)) = VFunM 99 (f6 e)
        f1 x = error ("Bad first arg to '||': "++show x)

        f2 True v = v
        f2 False v = to True

        f3 TruthP v k = k v
        f3 AbsurdP v k = k(VNS (NSProp TruthP))
        f3 x (VNS (NSProp y)) k = k(VNS (NSProp (implyP x y)))
        f3 x (VNS (NSYices e)) k = error ("No lifting from Prop to Yices Yet")
        f3 x y k | valTrue y = k(VNS (NSProp (implyP x TruthP)))
        f3 x y k | valFalse y = k(VNS (NSProp (implyP x AbsurdP)))                
        f3 _ x k = error ("Bad second arg to '||': "++show x)

        f4 e y | valTrue y = (VNS (NSYices (e :=> LitB True)))
        f4 e y | valFalse y = (VNS (NSYices (e :=> LitB False)))
        f4 e (VNS (NSYices x)) = (VNS (NSYices (e :=> x)))
        f4 e x = error ("Bad first second to '||': "++show x) 
        
        f6 b1 (VNS(NSBdd b2)) k = do { b3 <- implyBdd b1 b2; k(to b3)}          
        


notV = VFunM 99 notF
notF x k | valTrue x = k falseV
notF x k | valFalse x = k trueV
notF (VNS (NSProp x)) k = k(VNS (NSProp (notP x)))  
notF (VNS (NSYices e)) k = k (VNS (NSYices (NOT e)))
notF (VNS (NSBdd e)) k = do { y <- notBdd e; k (to y)}
notF v k = error ("Bad arg to 'not': "++show v)   
notVal x = notF x return
-----------------------------------------------
-- other miscellaneous functions

showV = VFun f where f v = to(show v)


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
    where pairs = (DM.toList set)
          f (key,t) = g (tuple key,simple t,None)
          g ([x,y],str,col) = (get d1 x,get d1 y,str,col)
          get (Dim n b xs) i = unJust(elemIndex i xs)
          unJust(Just x) = x
  edges (FA ds set) = error "Binary relations only"


instance Simple a => Graph (FiniteSet Bool,FiniteSet a) where
  nodes (x@(FA [d1,d2] set1),y@(FA [c1,c2] set2)) = map f es
    where getNodes ([(i,labelI),(j,labelJ)],v) = [(i,labelI),(j,labelJ)]
          getColor ([(i,node),(j,color)],v) = (i,color)
          es = nub (concat (map getNodes (zap x)))
          cs = map getColor (zap y)
          f (i,label) = case lookup i cs of
                          Just c -> (i,simple label,str2Color(simple c),Circle)      
  edges (x@(FA [d1,d2] set1),y@(FA [c1,c2] set2)) = edges x
    
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



zap :: FiniteSet t -> [([(Int, Literal)], t)]
zap (FA ds set) = map f (DM.toList set)
  where f (key,x) = (zipWith get ds (tuple key),x)
        get (Dim n b xs) lit = (unJust(elemIndex lit xs),lit)
        unJust(Just x) = x


------------------------------------------------------

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
typeNameNS (NSBdd t) = "Bdd"

applyNS apply1V (VNS(NSProp t)) arg k = error ("Should not happen prop Fun2")
applyNS apply1V f   (VNS(NSProp c)) k = error ("Should not happen prop Arg2")

reify:: Value -> ExpY
reify (VNS (NSYices e)) = e
reify x | valTrue x = LitB True
reify x | valFalse x = LitB False
reify (VBase (LInt n)) = LitI n
reify (VBase (LDouble n)) = LitR(approxRational n 0.0001)
reify (VTuple vs) = MKTUP(map reify vs)
reify v = error ("reify not defined for: "++show v)


----------------------------------------------------------------------------
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

--------------------------------------------------------

{- 
constraintToV :: (forall b . BooleanM IO b => FiniteSet b -> IO b) -> Value
constraintToV f = VFunM 99 g
  where g:: (forall a. Value -> (Value -> IO a) -> IO a)
        g (VSet BoolI set) k = do { z <- f set; k(to z)}
        g (VSet PropI set) k = do { z <- f set; k(VNS(NSProp z))}
        g (VSet BddI set) k = do { z <- f set;  k(VNS(NSBdd z))}
        g x k = error ("Bad input to 'full'\n   "++show x)

apply1V :: (forall b . BooleanM IO b => FiniteSet b -> IO (FiniteSet b)) -> Value -> IO Value
apply1V f (VSet BoolI set) = do { z <- (f set); return(VSet BoolI z)}
apply1V f (VSet PropI set) = do { z <- f set; return(VSet PropI z)}
apply1V f (VSet BddI set) = do { z <- f set; return(VSet BddI z)}
apply1V f x = error ("Bad input to 'apply1V'\n   "++show x)


apply2V:: (forall b . BooleanM IO b => FiniteSet b -> FiniteSet b -> IO b) -> Value
apply2V f = VFunM 99 g1
  where g1 ::  (forall a. Value -> (Value -> IO a) -> IO a)
        g1 (VSet BoolI x) k = k (VFunM 99 (g2 x))
        g1 (VSet PropI x) k = k (VFunM 99 (g3 x))
        g1 (VSet BddI x) k = k(VFunM 99 (g4 x))
        g1 x k = error ("1. Non FiniteSet argument"++show x)
  
        g2 :: FiniteSet Bool -> (forall a. Value -> (Value -> IO a) -> IO a)
        g2 x (VSet BoolI y) k = do { z <- f x y; k(to z)}
        g2 x y k = error ("2. Non FiniteSet argument"++show y)
        
        g3 :: FiniteSet (Prop Int) -> (forall a. Value -> (Value -> IO a) -> IO a)
        g3 x (VSet PropI y) k = do { z <- f x y; k(to z)} 
        g3 x (VSet BoolI y) k = do { z <- f x (boolToProp y); k(to z)} 
        g3 x y k = error ("3. Non FiniteSet argument"++show y)
        
        g4 :: FiniteSet (Bdd Int) -> (forall a. Value -> (Value -> IO a) -> IO a)
        g4 x (VSet BddI y) k = do { z <- f x y; k(to z)} 
        g4 x (VSet BoolI y) k = do { z <- f x (boolToBdd y); k(to z)} 
        g4 x (VSet PropI y) k = do { y2 <- propToBdd y; z <- f x y2; k(to z)} 
        g4 x y k = error ("4. Non FiniteSet argument"++show y)
        
        
-- fullV = constraintToV full
-- someV = constraintToV some
-- noneV = constraintToV none
-- oneV = constraintToV FiniteSet.one
-- projVal xs = apply1V (project xs)
-- subsetV = apply2V subsetM
-}

-------------------------------------------------------
   
------------------------------------------------------------------
-- we'd like to lift overloaded operations on FiniteSets to
-- Values, since sets can be represented mulitiple ways, we need to
-- test, and make changes in representation where necessary.
-- We have both unary and binary operations.

apply1 :: (forall b . (PP b,BooleanM IO b) => SetI b -> FiniteSet b -> IO a) -> 
          Value -> IO a
apply1 f (VSet BoolI s) = f BoolI s
apply1 f (VSet PropI s) = f PropI s
apply1 f (VSet BddI s) = f BddI s 
apply1 f v = error ("Non FiniteSet argument in apply1\n   "++show v)


apply2 :: (forall b .(BooleanM IO b) => SetI b -> FiniteSet b -> FiniteSet b -> IO a) -> 
           Value -> Value -> IO a
apply2 f (VSet BoolI r) (VSet BoolI s) = f BoolI r s
apply2 f (VSet BoolI r) (VSet PropI s) = f PropI (boolToProp r) s
apply2 f (VSet BoolI r) (VSet BddI s) = f BddI (boolToBdd r) s 
apply2 f (VSet PropI r) (VSet BoolI s) = f PropI r (boolToProp s)
apply2 f (VSet PropI r) (VSet PropI s) = f PropI r s
apply2 f (VSet PropI r) (VSet BddI s) = do { r2 <- propToBdd r; f BddI r2 s }
apply2 f (VSet BddI r)  (VSet BoolI s) = f BddI r (boolToBdd s)
apply2 f (VSet BddI r)  (VSet PropI s) = do { s2 <- propToBdd s; f BddI r s2 }
apply2 f (VSet BddI r)  (VSet BddI s) = f BddI r s
apply2 f x y = error ("Non FiniteSet arguments in apply2\n   "++show x++"\n   "++show y)

-- Once we have lifted the operations on FiniteSets to operations
-- on Values we need to lift these Value to Value functions into
-- Primitive functions inside FunLog, be embedding them in VFun and VFunM

binaryToValue :: (Value -> Value -> IO Value) -> Value
binaryToValue f = VFun g
  where g v = VFunM 99 (h v)
        h :: Value -> (forall a. Value -> (Value -> IO a) -> IO a)
        h v u k = do {  x <- f v u; k x } 
        
unaryToValue f = VFunM 99 h   
  where h :: (forall a. Value -> (Value -> IO a) -> IO a)
        h u k = do {  x <- f u; k x } 
                 
-----------------------------------------------                 
-- Now the liftings

-- Set operations
complementVal :: Value -> IO Value
complementVal = apply1 (\ i x -> lift1M (toFLSet i) (complement x))
complementV = unaryToValue complementVal

unionVal :: Value -> Value -> IO Value
unionVal = apply2 (\ i x y -> lift1M (toFLSet i) (union x y))
unionV = binaryToValue unionVal

intersectVal :: Value -> Value -> IO Value
intersectVal = apply2 (\ i x y -> lift1M (toFLSet i) (intersect x y))
intersectV = binaryToValue intersectVal

differenceVal :: Value -> Value -> IO Value
differenceVal = apply2 (\ i x y -> lift1M (toFLSet i) (difference x y))
differenceV = binaryToValue differenceVal

-- relational operations

projVal xs = apply1 (\ i x -> lift1M (toFLSet i) (project xs x))

-- lift1M f x = do { a <- x; return(f a)}

projectVal :: [Int] -> Value -> IO Value
projectVal fields = apply1 (\ i s -> lift1M (VSet i) (project fields s))

projectV:: Value
projectV = VFun f
  where f v = case isIntList v of
                Just ns -> unaryToValue(projectVal ns)
                Nothing -> error ("Non [Int] as first arg to 'project'\n   "++show v)

selectVal :: (Key -> Bool) -> Value -> IO Value
selectVal p = apply1 (\ i s -> return(toFLSet i (select p s)))

selectValM :: (Key -> IO Bool) -> Value -> IO Value
selectValM p = apply1 (\ i s -> do { a <- (selectM p s); return(toFLSet i a)})

selectV = (VFunM 99 f)
  where f predf k = k(unaryToValue(selectValM (extract predf)))
        extract :: Value -> Key -> IO Bool
        extract (VFunM _ f) = \ key -> f(VTuple(map to (tuple key))) (return . from)
        extract (VFun f) = \ key -> return(from(f(VTuple(map to (tuple key)))))
        extract v = error ("Non function as first arg to select\n   "++show v)

joinVal :: Int -> Value -> Value -> IO Value
joinVal n = apply2 (\ i x y -> lift1M (toFLSet i) (join n x y))

joinV = VFunM 99 f
  where f (VBase (LInt n)) k = k(binaryToValue (joinVal n))
        f v k = error ("First arg to 'join' is not an Int: "++show v)

 
-- Constraint operations

subsetVal :: Value -> Value -> IO Value
subsetVal = apply2 (\ i x y -> lift1M (toFLProp i) (subsetM x y))
subsetV = binaryToValue subsetVal

fullVal :: Value -> IO Value
fullVal = apply1 (\ i x -> lift1M (toFLProp i) (full x))
fullV = unaryToValue fullVal

someVal :: Value -> IO Value
someVal = apply1 (\ i x -> lift1M (toFLProp i) (some x))
someV = unaryToValue someVal

noneVal :: Value -> IO Value
noneVal = apply1 (\ i x -> lift1M (toFLProp i) (none x))
noneV = unaryToValue noneVal

oneVal :: Value -> IO Value
oneVal = apply1 (\ i x -> lift1M (toFLProp i) (FiniteSet.one x))
oneV = unaryToValue oneVal



