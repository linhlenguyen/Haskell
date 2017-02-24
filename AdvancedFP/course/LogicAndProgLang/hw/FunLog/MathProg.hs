{-# LANGUAGE TupleSections #-}
module MathProg where

import Data.List(sort,sortBy,nub)
import Range
import MPS
import System.Cmd(rawSystem,system)


clpPath = "D:\\FreyaDownloads\\CLPsolver\\bin\\clp.exe"


-- A polynomial is a list of
-- (abstract variable,coefficient)

type PolyNom n = [(String,n)]

-- The meaning of an MExp is
-- a PolyNomial with an additive 
-- constant. The polynomial may 
-- have no polynomial terms

data MExp n = Term (PolyNom n) n

data Rel n 
  = RANGE (PolyNom n) (Range n)
  | TAUT   -- True
  | UNSAT  -- False 
 deriving Eq

p1 = [("x",5),("y",2)]
t1 = Term p1 (5::Int)
t2 = Term [("z",3)] (2::Int)
-- r1 = RANGE p1 (joinR (greaterThan 4) (lessThanOrEq 20))

noRange = Range MinusInf PlusInf 
-----------------------------------------------
-- operations over MExp

plusM (Term [] a) (Term [] b) = Term [] (a+b)
plusM (Term [] a) (Term ys b) = Term ys (a+b)
plusM (Term xs a) (Term [] b) = Term xs (a+b)   
plusM (Term xs a) (Term ys b) = Term (mergeP (+) xs ys) (a+b)  

-- in a Sum, if the same indexed variables appears twice, 
-- we add the coefficients

mergeP:: (Num n,Eq n) => 
     (n -> n -> n) -> PolyNom n -> 
     PolyNom n -> PolyNom n  
mergeP f [] ys = ys
mergeP f xs [] = xs
mergeP f ((x,n):xs)((y,m):ys)=
 case compare x y of
  EQ -> case (f n m) of
         0 -> mergeP f xs ys
         i -> (x,i):mergeP f xs ys
  LT -> (x,n): 
        mergeP f xs ((y,m):ys)
  GT -> (y,m):
        mergeP f ((x,n):xs) ys

minusM x y = plusM x (negM y)
negM (Term xs a) = Term [(name,negate x) | (name,x) <- xs] (negate a)

timesM (Term [] a) (Term [] b) = Term [] (a*b)
timesM (Term [] a) (Term ys b) = Term (multiply ys a) (a*b)
timesM (Term xs a) (Term [] b) = Term (multiply xs b) (a*b)   
timesM (Term xs a) (Term ys b) = error "UnSupported" 

-- We can multiply the coefficients of a Polynomial
multiply set n = [(name,n * x) | (name,x) <- set]

---------------------------------------------------------
-- Creating a (Rel n) from a comparison of two MExp

negPoly xs = [(name,negate x) | (name,x) <- xs]

lteqM (Term [] a) (Term [] b) = 
   if (a <= b) then [TAUT] else [UNSAT] 
lteqM (Term [] a) (Term xs b) = 
   [RANGE xs (Range (LtEQ(a-b)) PlusInf)]  
lteqM (Term xs a) (Term [] b) = 
   [RANGE xs (Range MinusInf (LtEQ (b-a)))]  
lteqM (Term xs a) (Term ys b) = 
  [RANGE (mergeP (+) xs (negPoly ys)) 
         (Range MinusInf (LtEQ (b-a)))]
         
ltM (Term [] a) (Term [] b) = 
   if (a < b) then [TAUT] else [UNSAT] 
ltM (Term [] a) (Term xs b) = 
   [RANGE xs (Range (Lt(a-b)) PlusInf)] 
ltM (Term xs a) (Term [] b) = 
   [RANGE xs (Range MinusInf (Lt (b-a)))]  
ltM (Term xs a) (Term ys b) = 
  [RANGE (mergeP (+) xs (negPoly ys)) 
         (Range MinusInf (Lt (b-a)))]  

gteqM t1 t2 = lteqM t2 t1
gtM t1 t2 = ltM t2 t1         

eqM (Term [] a) (Term [] b) = 
   if (a == b) then [TAUT] else [UNSAT]
eqM (Term [] a) (Term xs b) = 
  [RANGE xs (Range (LtEQ(a-b)) (LtEQ(a-b)))]  
eqM (Term xs a) (Term [] b) = 
  [RANGE xs (Range (LtEQ(b-a)) (LtEQ(b-a)))]  
eqM (Term xs a) (Term ys b) = 
  case (mergeP (+) xs (negPoly ys)) of
    (zs@[]) -> if (b-a)==0 then [TAUT] else [UNSAT]
    zs -> [RANGE zs (Range (LtEQ(b-a)) (LtEQ(b-a)))]

-----------------------------------------------------
-- Anding together two [Rel Int]

cmpRel UNSAT UNSAT = EQ
cmpRel UNSAT _ = LT
cmpRel TAUT TAUT = EQ
cmpRel TAUT _ = LT
cmpRel (RANGE xs _) (RANGE ys _) = compare xs ys

instance Ord n => Ord (Rel n) where
  compare = cmpRel 

andM xs ys = help (sort xs) (sort ys)  
  where help (UNSAT:_) ys = [UNSAT]
        help xs (UNSAT:_) = [UNSAT]
        help (TAUT: xs) ys = help xs ys
        help xs (TAUT: ys) = help xs ys
        help [] ys = ys
        help xs [] = xs
        help (RANGE x a:xs) (RANGE y b:ys) 
           | x == y = RANGE x (intersectRange a b) : help xs ys
        help (RANGE x a:xs)(ys@(RANGE y b:_)) 
           | x < y  = RANGE x a:(help xs ys)
        help (xs@(RANGE x a:_))(RANGE y b:ys) 
           | x > y  = RANGE y b:(help xs ys)

    
---------------------------------------------------------

-- turning a PolyNom into a string

showPoly xs close = plistf shCoef "(" xs " + " close
  where plistf :: (a -> String) -> String -> [a] -> String -> String -> String
        plistf f open xs sep close = open ++ help xs ++ close
           where help [] = ""
                 help [x] = f x
                 help (x:xs) = f x ++ sep ++ help xs   
        shCoef (s,n) = show n++s -- s++"*"++show n

showMExp (Term [] n) = show n
showMExp (Term [(x,1)] 0) = x
showMExp (Term xs 0) = showPoly xs ")"
showMExp (Term xs n) = showPoly xs (" + "++show n++")")

instance (Eq n,Num n,Show n) => Show (MExp n) where
  show x = showMExp x
  
instance Show n => Show (Rel n) where
  show (RANGE xs s) = showRange s (show xs)
  show (TAUT) = "T"
  show (UNSAT) = "F" 

rng (RANGE xs (Range l h)) = [x,y]++ map g xs ++ [a,b]
  where (x,y) = showLower l
        (a,b) = showUpper h
        g (x,n) = show n
        
shRng (RANGE xs s) = showRange s (showPoly xs ")")
shRng (TAUT) = "T"
shRng (UNSAT) = "F" 

----------------------------------------------------------
-- putting things in standard format

mexpVars (Term xs n) = nub(sort(map fst xs))
relVars (RANGE xs rng) = map fst xs
relVars TAUT = []
relVars UNSAT = []
boolVars xs = nub(sort(concat(map relVars xs)))

-- Both the full set of names, and the coefficient 
-- pair list must be in lexographic order

fillInMissing [] xs = xs
fillInMissing vs [] = map (,0) vs
fillInMissing (v:vs) ((x,n):xs)
  | x==v = (x,n): fillInMissing vs xs
  | x<v  = (x,n): fillInMissing (v:vs) xs
  | x>v  = (v,0): fillInMissing vs ((x,n):xs)
  
fill vs (Term xs c) = Term (fillInMissing vs xs) c
fillR vs (RANGE xs rng) = RANGE (fillInMissing vs xs) rng
fillR vs TAUT = TAUT
fillR vs UNSAT = UNSAT

fillB vs xs = map (fillR vs) xs

strs vs xs = (top,side,map rng xs)
  where top = ["",""]++vs++["",""]
        side = replicate (length xs) ""

fillProb objective constraints = (objective2,constraints2,strings)
  where vs = nub(sort(mexpVars objective++boolVars constraints))
        objective2 = fill vs objective
        constraints2 = fillB vs constraints
        strings = strs vs constraints2
        

-------------------------------------------------------

mpsPrint nm cost vs rs = printMPS (toMPS 1 nm cost vs rs)

toMPS :: (Eq n,Num n) => n -> String -> MExp n -> [String] -> [Rel n] -> [MPS n]
toMPS epsilon nm (cost@(Term ps c)) vs rs = 
         [NAME nm
         ,ROWS (Row N "COST" : rows)
         ,COLUMNS (map getCol vs)
         ,RHS "RHS" rhs
         ,BOUNDS "BND1" bounds
         ,ENDATA]
  where g i r = ("R"++show i,r)
        pairs = (zipWith g [1..] rs)
        zs = map (toRow epsilon) pairs        
        acc (Left xs) (rs,cs,bs) = (r++rs,c++cs,bs) where (r,c) = unzip xs
        acc (Right xs) (rs,cs,bs) = (rs,cs,xs++bs)
        (rows,rhs,bounds) = foldr acc ([],[],[]) zs
        getCol c = (c,concat(map (toCol c) (("COST",RANGE ps noRange):pairs)))
        
-- toCol :: String -> (t, Rel Int) -> [(t, Number)]
toCol c (r,TAUT) = []
toCol c (r,UNSAT) = []
toCol c (r,RANGE [(x,1)] rng) = [] -- unit polynomials are bounds not coefficients
toCol c (r,RANGE ps rng) = (coeff c r ps)

coeff c r ps = 
   case lookup c ps of
     Nothing -> []
     Just i -> [(r, i)]

f epsilon r (LtEQ n) = (r,n)
f epsilon r (Lt n) = (r,(n+epsilon))

-- Return Either (Row,Col-entry) BndRng
-- A singleton variable is Bounderd
-- A non-unit polynomial leads to a row and some columns

-- toRow :: (RowName, Rel Int) -> (Row, (RowName, Number))
toRow epsilon (r,TAUT) = error "ALWAYS TRUE"
toRow epsilon (r,UNSAT) = error "ALWAYS FALSE"
toRow epsilon (r,RANGE [(x,1)] (Range MinusInf (LtEQ n))) = Right[(UP,x, n)]
toRow epsilon (r,RANGE [(x,1)] (Range MinusInf (Lt n)))   = Right[(UP,x,n-epsilon)]
toRow epsilon (r,RANGE [(x,1)] (Range (LtEQ n) PlusInf))  = Right[(LO,x, n)]
toRow epsilon (r,RANGE [(x,1)] (Range (Lt n)   PlusInf))  = Right[(LO,x,n+epsilon)]
toRow epsilon (r,RANGE [(x,1)] (Range (LtEQ n) (LtEQ m))) = Right[(LO,x, n),(UP,x, m)]
toRow epsilon (r,RANGE [(x,1)] (Range (Lt   n) (LtEQ m))) = Right[(LO,x,n+epsilon),(UP,x, m)]
toRow epsilon (r,RANGE [(x,1)] (Range (LtEQ n) (Lt   m))) = Right[(LO,x,n),(UP,x,m-epsilon)]
toRow epsilon (r,RANGE ps (Range MinusInf (LtEQ n))) = Left [(Row L r,(r,n))]
toRow epsilon (r,RANGE ps (Range MinusInf (Lt n)))   = Left [(Row L r,(r,n-epsilon))]
toRow epsilon (r,RANGE ps (Range (LtEQ n) PlusInf))  = Left [(Row G r,(r, n))]
toRow epsilon (r,RANGE ps (Range (Lt n)   PlusInf))  = Left [(Row G r,(r,n+epsilon))]
toRow epsilon (r,RANGE ps (Range (LtEQ n) (LtEQ m))) | n==m = Left [(Row E r,(r, n))]
toRow epsilon (r,RANGE ps (Range (LtEQ n) (LtEQ m))) = Left[(Row G (less r),(less r,n))
                                                           ,(Row L (more r),(more r,n))]
toRow epsilon (r,RANGE ps (Range (Lt   n) (LtEQ m))) = Left[(Row G (less r),(less r,n+epsilon))
                                                           ,(Row L (more r),(more r,n))]
toRow epsilon (r,RANGE ps (Range (LtEQ n) (Lt   m))) = Left[(Row G (less r),(less r,n))
                                                           ,(Row L (more r),(more r,n-epsilon))]
toRow epsilon (r,RANGE ps (Range (Lt   n) (Lt   m))) = Left[(Row G (less r),(less r,n+epsilon))
                                                           ,(Row L (more r),(more r,n-epsilon))]
                                                                   
less r = r++"L"
more r = r++"R"

----------------------------------------------------------------------
-- Running the solver

solveMPS mode mpsFile solFile nm cost vs rs = 
   do { let mps = toMPS 1 nm cost vs rs
            command = clpPath ++ mpsFile++" "++mode++" -primalS -solution "++solFile
      ; putStrLn ("\n****** COMMAND = "++command++" ******")      
      ; writeFile mpsFile (unlines(showMPS mps))
      ; putStrLn ("Executing: "++command)
      ; rawSystem clpPath [mpsFile,mode,"-primalS","-solution",solFile]
      ; vs <- clpFile solFile
      ; return vs
      }

writeMpsFile file nm cost vs rs = 
   writeFile file (unlines(showMPS(toMPS 1 nm cost vs rs)))


   



