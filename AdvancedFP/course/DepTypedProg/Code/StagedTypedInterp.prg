
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
  Iconst:: Int -> Term Int
  Bconst:: Bool -> Term Bool
  Sconst:: String -> Term String
  Cconst:: Char -> Term Char
  If:: Term Bool -> Term a -> Term a -> Term a
  Binary:: BinOp a b c -> Term a -> Term b -> Term c
  Unary:: UnOp a b -> Term a -> Term b
  


eval:: Term a -> a
eval (Iconst i) = i
eval (Bconst b) = b
eval (Sconst s) = s
eval (Cconst c) = c
eval (If tst thn els) =
  case eval tst of
    (True) -> eval thn
    (False) -> eval els   
eval (Binary oper x y) =
  case (oper,eval x,eval y) of
    (Plus, a, b) -> (a+b)
    (Minus, a, b) -> (a-b)
    (Times, a, b) -> (a*b)
    (App,a,b) -> (a ++ b)
eval (Unary oper x) =
  case (oper,eval x) of
    (ZeroP, a) -> (a==0)
    (EmptyP,a) -> (null a)
    (Head,(x:xs)) -> x
    (Tail,(x:xs)) -> xs
    (Not,x) -> (not x)
   
  