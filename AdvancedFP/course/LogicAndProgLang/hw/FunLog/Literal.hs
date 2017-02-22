{-# LANGUAGE DeriveDataTypeable #-}

module Literal where

import Data.Typeable
import Auxfuns
import Data.List(null)

----------------------------------------------------------------
-- We can make "FiniteSet" for "Base" types. We store elements
-- of these types as "Literal". A "Dim" is a finite set of
-- some "Base" type. We store a list of "Literal" inside
-- representing this set.

data Base = Int | String | Char | Double | Bool | Enum String | Tuple [Base]
  deriving Eq
  
data Literal 
   = LInt { unInt :: Int }         -- 5 , -5
   | LDouble { unDouble::Double }  -- 2.3 , 0.5 , -3.4
   | LChar { unChar::Char }        -- 'x'
   | LString {unString::String}    -- "\nabc"S@
   | LUnit
   | LCon String String        -- True, False, constants of enumerated types 
                               -- i.e. a type where ALL constructors are Nullary!
   deriving (Typeable,Ord)                               


instance Show Base where
  show Int = "Int"
  show Char = "Char"
  show Double = "Double"
  show String = "String"
  show Bool = "Bool"
  show (Enum s) = s
  show (Tuple xs) = plistf show "#(" xs "," ")"

instance Show Literal where
  show (LInt n) = show n
  show (LDouble n) = show n
  show (LChar c) = show c
  show (LString s) = ['"']++s++['"']
  show (LCon tnm name) = name
  show LUnit = "()"

instance Simple Literal where
  simple (LInt n) = simple n
  simple (LDouble d) = show d
  simple (LChar c) = simple c
  simple (LString s) = simple s
  simple LUnit = "()"
  simple (LCon tnm c) = c
  

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

  
instance Num Literal where
  (+) (LInt n) (LInt m) = LInt(n+m)
  (+) (LDouble n) (LDouble m) = LDouble(n+m)  
  (*) (LInt n) (LInt m) = LInt(n*m)
  (*) (LDouble n) (LDouble m) = LDouble(n*m)  
  (-) (LInt n) (LInt m) = LInt(n+m)
  (-) (LDouble n) (LDouble m) = LDouble(n+m)  
  abs (LInt n) = LInt(abs n)
  abs (LDouble n) = LDouble(abs n)
  negate (LInt n) = LInt(negate n)
  negate(LDouble n) = LDouble(negate n)
  signum (LInt n) = LInt(signum n)
  signum (LDouble n) = LDouble(signum n)
  fromInteger n = LInt(fromInteger n)
    
instance Eq Literal where
  (LInt n) == (LInt m) = n==m
  (LDouble n) == (LDouble m) = n==m
  (LChar n) == (LChar m) = n==m
  (LString n) == (LString m) = n==m
  (LCon t x) == (LCon s y) = x==y  
  LUnit == LUnit = True
  _ == _ = False
  

toLiteral:: (Show t,Typeable t) => t -> Literal
toLiteral x = tryAll [try LInt, try useInteger, try LDouble, try LChar, try LString, try un] x
  where un () = LUnit
        useInteger :: Integer -> Literal
        useInteger n = LInt (fromIntegral n)

tryAll [] x = LCon (tname x) (show x)
tryAll (f:fs) x = 
   case f x of
     Just y -> y
     Nothing -> tryAll fs x
             
try f x = case cast x of
           Just i -> Just (f i)
           _ ->  Nothing   
  
tname x = show(fst(splitTyConApp (typeOf x))) 

-- operations between Base types and Literals

baseType :: Typeable t => t -> Base
baseType x = repToBase(typeOf x)

repToBase rep = 
     case (show con,args) of
       ("Int",[]) -> Int
       ("Char",[]) -> Char
       ("Double",[]) -> Double
       ("Bool",[]) -> Bool
       ("()",[]) -> Tuple[]
       ("(,)",[x,y]) -> Tuple[repToBase x,repToBase y]
       ("(,,)",[x,y,z]) -> Tuple[repToBase x,repToBase y,repToBase z]
       ("(,,,)",[w,x,y,z]) -> Tuple[repToBase w,repToBase x,repToBase y,repToBase z]
       ("(,,,,)",[v,w,x,y,z]) -> Tuple[repToBase v,repToBase w,repToBase x,repToBase y,repToBase z] 
       ("Integer",[]) -> error ("Integer is not a base type.")
       (name,args)
          | ((show rep)=="[Char]") && (length args == 1) -> String
          | null args -> Enum name
          | True -> error ("No base type for: "++show rep)
  where (con,args) = splitTyConApp rep
  

        
  
  
    
