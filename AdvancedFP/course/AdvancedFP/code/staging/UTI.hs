{-# LANGUAGE   TemplateHaskell, GADTs, 
    KindSignatures, ScopedTypeVariables #-}

module UTI where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax(lift)
import Language.Haskell.TH.PprLib

-------------------------------------------
-- Staging specific helper functions

-- For inspecting a generated piece of code.

sh:: Ppr a => Q a -> IO ()
sh x = (runQ (do { a <- x; return (show (ppr a))})) >>= putStrLn

-------------------------------------------

data Expr:: * where
  Variable:: String -> Expr
  Constant:: Int -> Expr
  Truth:: Bool -> Expr
  Add:: Expr
  Less:: Expr
  Apply:: Expr -> Expr -> Expr
  Tuple:: [Expr] -> Expr 


exp1 = Tuple
       [ Apply Less (Tuple [Constant 7,Constant 4])
       , Apply Add
            (Tuple [Variable "x",Variable "y"])
       ]
  
data Value :: * where
  IntV:: Int -> Value
  BoolV:: Bool -> Value
  FunV:: (Value -> Value) -> Value
  TupleV :: [Value] -> Value  

plus (TupleV[IntV n ,IntV m]) = IntV(n+m)
less (TupleV[IntV n ,IntV m]) = BoolV(n < m)
apply (FunV f) x = f x

eval:: (String -> Value) -> Expr -> Value
eval env (Variable s) = env s
eval env (Constant n) = IntV n
eval env Add = FunV plus
eval env Less = FunV less
eval env (Apply f x) =
   case eval env f of
    FunV g -> g (eval env x)
eval env (Tuple xs) = TupleV(map (eval env) xs)   


stagedEval:: (String -> Q Exp) -> Expr -> Q Exp
stagedEval env (Variable s) = env s
stagedEval env (Constant n) = [| IntV n |]
stagedEval env Add = [| FunV plus |]
stagedEval env Less = [| FunV less |]
stagedEval env (Tuple xs) = 
      [| TupleV $(mapLift (stagedEval env) xs) |]
  where mapLift f [] = [| []   |]
        mapLift f (x:xs) = [| $(f x) : $(mapLift f xs) |]
stagedEval env (Apply f x) =
  [| apply $(stagedEval env f) $(stagedEval env x) |]
  
  
env "x" = [| IntV 99 |]
env "y" = [| IntV 49 |]
env z = error ("Unknown variable: "++z)

splice1 = stagedEval env  exp1


---------------------------------------------
-- A typed language term with NO variables
---------------------------------------------

data Term:: * -> * where
  Iconst:: Int -> Term Int
  Bconst:: Bool -> Term Bool
  Sconst:: String -> Term String
  Cconst:: Char -> Term Char
  If:: Term Bool -> Term a -> Term a -> Term a
  Binary:: BinOp a b c -> Term a -> Term b -> Term c
  Unary:: UnOp a b -> Term a -> Term b
  
data BinOp:: * -> * -> * -> * where
 Plus:: BinOp Int Int Int
 Minus:: BinOp Int Int Int
 Times:: BinOp Int Int Int
 App:: BinOp String String String
 
data UnOp :: * -> * -> * where
  ZeroP:: UnOp Int Bool
  EmptyP:: UnOp String Bool
  Head:: UnOp String Char
  Tail:: UnOp String String
  Not:: UnOp Bool Bool 
  
-- A typed interpretor

teval:: Term a -> a
teval (Iconst i) = i
teval (Bconst b) = b
teval (Sconst s) = s
teval (Cconst c) = c
teval (If tst thn els) =
  case (teval tst) of
    (True) -> (teval thn)
    (False) -> (teval els)  
teval (Binary oper x y) =
  case (oper,teval x,teval y) of
    (Plus, a, b)  -> a + b
    (Minus, a, b) -> a - b
    (Times, a, b) -> a * b
    (App,a,b) -> a ++ b
teval (Unary oper x) =
  case (oper,teval x) of
    (ZeroP, a) -> a == 0 
    (EmptyP,a) -> null a 
    (Head,y) -> let head (x:xs) = x in head y
    (Tail,y) -> let tail (x:xs) = xs in tail y
    (Not,x) -> not x
    
-- A staged version of a typed interpreter    

seval:: Term a -> Q Exp
seval (Iconst i) = lift i
seval (Bconst b) = lift b
seval (Sconst s) = lift s
seval (Cconst c) = lift c
seval (If tst thn els) =
  [| case $(seval tst) of
       (True) -> $(seval thn)
       (False) -> $(seval els)  |]
seval (Binary oper x y) =
  case (oper,seval x,seval y) of
    (Plus, a, b) -> [| $a + $b |]
    (Minus, a, b) -> [| $a - $b |]
    (Times, a, b) -> [| $a * $b |]
    (App,a,b) -> [| $a ++ $b |]
seval (Unary oper x) =
  case (oper,seval x) of
    (ZeroP, a) -> [| $a == 0 |]
    (EmptyP,a) -> [| null $a |]
    (Head,y) -> [| let head (x:xs) = x in head $y |]
    (Tail,y) -> [| let tail (x:xs) = xs in tail $y |]
    (Not,x) -> [| not $x |]
    
exp2 = If (Unary EmptyP (Sconst "abc"))
          (Binary App (Sconst "123") (Sconst "xyz"))
          (Unary Tail (Sconst "mnop"))

splice2 = seval exp2

----------------------------------------
-- here is the staged nway zip

data Zip :: * -> * -> * where
  Z:: Zip a [a]
  S:: Zip b c ->  Zip (a -> b) ([a] -> c)
  
help:: Zip a b -> a -> b -> b
help Z x xs = x:xs
help (S n) f rcall =
 (\ ys -> case ys of
           (z:zs) -> help n (f z) (rcall zs)
           other -> skip n)           

skip:: Zip a b -> b
skip Z = []
skip (S n) = \ ys -> skip n

zipN:: Zip a b -> a -> b
zipN Z = \ n -> [n]
zipN (n@(S m)) = let zip f = help n f (\ x -> zip f x)
                 in zip

test5 = zipN (S(S(S Z))) (\ x y z -> (x+z,y))[2,3] [5,1] [6,8]

-- Now to stage it we unfold the Z and (S n) cases
-- but build code for the rest.


skipC:: Zip a b -> Q Exp
skipC Z = [e| [] |]
skipC (S n) = [| \ ys -> $(skipC n) |]


helpC:: Zip a b -> Q Exp -> Q Exp -> Q Exp
helpC Z x xs = [| $x : $xs |]
helpC (S n) f rcall =
 [| \ ys -> case ys of
             (z:zs) -> $(helpC n [| $f z |] [| $rcall zs |])
             other -> $(skipC n) |]
             
zipND:: Zip a b -> Q [Dec]
zipND Z = [d| zip n = [n]  |]
zipND (n@(S m)) = 
  [d| zip f = $(helpC n [| f |] [| zip f |])
  |]
     
zipNC:: Zip a b -> Q Exp
zipNC Z = [| \ n -> [n]  |]
zipNC (n@(S m)) = 
  [| let  
         zip f = $(helpC n [| f |] [| zip f |])
     in zip |]     
     
splice3 = zipNC (S (S (S Z)))

splice4 = zipND (S (S (S Z)))


arr x y = ArrowT `AppT` x `AppT` y

zipType :: [Name] -> [Name] -> Zip t t1 -> ([Name], Type,Type)
zipType bound (nm:nms) Z = (nm:bound,VarT nm,AppT ListT (VarT nm))
zipType bound (nm:nms) (S n) = (bnd,arr a b,arr(AppT ListT a) c)
  where (bnd,b,c) = zipType (nm:bound) nms n
        a = VarT nm
        
names = map (mkName . (:[])) "abcdefghijklmnopqrstuvwxyz" 
zipT n = return(ForallT (map PlainTV bnd) [] (arr dom rng))
  where (bnd,dom,rng) = zipType [] names n
             
-----------------------------------------
plistf :: (a -> String) -> String -> [a] -> String -> String -> String
plistf f open xs sep close = open ++ help xs ++ close
  where help [] = ""
        help [x] = f x
        help (x:xs) = f x ++ sep ++ help xs    
  
instance Show Value where
  show (IntV n) = show n
  show (BoolV b) = show b
  show (FunV f) = "<fun>"
  show (TupleV vs) = plistf show "(" vs "," ")"