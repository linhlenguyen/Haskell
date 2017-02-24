data VNum:: Tag ~> *0 ~> Row Tag *0 ~> *0 where
  Zv:: VNum l t (RCons l t row)
  Sv:: VNum l t (RCons a b row) -> VNum l t (RCons x y (RCons a b row))
 deriving Nat(u)

data Exp2:: Row Tag *0 ~> *0 ~> *0 where
  Var:: Label v -> VNum v t e -> Exp2 e t
  Less:: Exp2 e Int -> Exp2 e Int -> Exp2 e Bool
  Add:: Exp2 e Int -> Exp2 e Int -> Exp2 e Int
  If:: Exp2 e Bool -> Exp2 e t -> Exp2 e t -> Exp2 e t

data Vector:: Row Tag *0 ~> *0  where
  VNil :: Vector RNil
  VCons :: Label t -> a -> Vector xs -> Vector (RCons t a xs)
  
eval:: Exp2 env t -> Vector env -> t
eval (Var v Zv) (VCons u value xs) = value
eval (Var v (Sv n)) (VCons _ _ xs) = eval (Var v n) xs
eval (Less x y) env = (eval x env) < (eval y env)
eval (Add x y) env = (eval x env) + (eval y env)
eval (If tst x y) env = if (eval tst env) then (eval x env) else (eval y env)
  
 