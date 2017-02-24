
data BinOp:: *0 where
 Plus:: BinOp
 Minus:: BinOp
 Times:: BinOp
 App:: BinOp
 
data UnOp :: *0 where
  ZeroP:: UnOp
  EmptyP:: UnOp
  Head:: UnOp
  Tail:: UnOp
  Not:: UnOp
 
data Term:: *0 where
  Iconst:: Int -> Term
  Bconst:: Bool -> Term
  Sconst:: String -> Term
  Cconst:: Char -> Term
  If:: Term -> Term -> Term -> Term
  Binary:: BinOp -> Term -> Term -> Term
  Unary:: UnOp -> Term -> Term
  
data Value:: *0 where
  I:: Int -> Value
  B:: Bool -> Value
  Str:: String -> Value
  Chr:: Char -> Value
  

eval:: Term -> Value
eval (Iconst i) = I i
eval (Bconst b) = B b
eval (Sconst s) = Str s
eval (Cconst c) = Chr c
eval (If tst thn els) =
  case eval tst of
    (B True) -> eval thn
    (B False) -> eval els
    other -> error "Non bool in test of if"    
eval (Binary oper x y) =
  case (oper,eval x,eval y) of
    (Plus, I a, I b) -> I(a+b)
    (Minus, I a, I b) -> I(a-b)
    (Times, I a, I b) -> I(a*b)
    (App,Str a,Str b) -> Str(a ++ b)
    other -> error "Ill-typed use of Binary operator."
eval (Unary oper x) =
  case (oper,eval x) of
    (ZeroP, I a) -> B(a==0)
    (EmptyP,Str a) -> B(null a)
    (Head,Str(x:xs)) -> Chr x
    (Tail,Str(x:xs)) -> Str xs
    (Not,B x) -> B(not x)
    other -> error "Ill-typed use of Unary operator."
  
  
    