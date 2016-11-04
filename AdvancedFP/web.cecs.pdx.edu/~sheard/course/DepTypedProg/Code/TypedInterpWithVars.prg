
data Rep:: *0 ~> *0 where
  Char :: Rep Char
  Int :: Rep Int
  String:: Rep String
  Bool:: Rep Bool

match:: Rep a -> Rep b -> Maybe(Equal a b)
match Char Char = Just Eq
match Int Int = Just Eq
match String String = Just Eq
match Bool Bool = Just Eq

data Var:: *0 ~> *0 where
  V:: String -> Rep a -> Var a
  
data Env:: *0 where
  Empty :: Env
  Add:: Var a -> a -> Env -> Env
 deriving Record(e)

e1 :: Env
e1 = {V "z" Int = 5, V "t" Bool = True}e

lookup:: Env -> Var t -> Maybe t
lookup Empty v = Nothing
lookup (Add (V s t1) v env) (V t t2) =
  case (eqStr s t,match t1 t2) of
    (True,Just Eq) -> (Just v)
    (_,_) -> lookup env (V t t2)
    

data BinOp:: *0 ~> *0 ~> *0 ~> *0 where
 Plus:: BinOp Int Int Int
 Minus:: BinOp Int Int Int
 Times:: BinOp Int Int Int
 App:: BinOp String String String
 

data UnOp :: *0 ~> *0 ~> *0 where
  ZeroP:: UnOp Int Bool
  EmptyP:: UnOp String Bool
  Head:: UnOp String Char
  Tail:: UnOp String String
  Not:: UnOp Bool Bool 


data Term:: *0 ~> *0 where
  Var:: Var a -> Term a
  Iconst:: Int -> Term Int
  Bconst:: Bool -> Term Bool
  Sconst:: String -> Term String
  Cconst:: Char -> Term Char
  If:: Term Bool -> Term a -> Term a -> Term a
  Binary:: BinOp a b c -> Term a -> Term b -> Term c
  Unary:: UnOp a b -> Term a -> Term b
  

eval:: Term a -> Env -> a
eval (Var v) env =
  case lookup env v of
    Just w -> w
    Nothing -> error ("unknown variable "++show v)
eval (Iconst i) env = i
eval (Bconst b) env = b
eval (Sconst s) env = s
eval (Cconst c) env = c
eval (If tst thn els) env =
  case eval tst env of
    (True) -> eval thn env
    (False) -> eval els env
eval (Binary oper x y) env =
  case (oper,eval x env,eval y env) of
    (Plus, a, b) -> (a+b)
    (Minus, a, b) -> (a-b)
    (Times, a, b) -> (a*b)
    (App,a,b) -> (a ++ b)
eval (Unary oper x) env =
  case (oper,eval x env) of
    (ZeroP, a) -> (a==0)
    (EmptyP,a) -> (null a)
    (Head,(x:xs)) -> x
    (Tail,(x:xs)) -> xs
    (Not,x) -> (not x)
   
  