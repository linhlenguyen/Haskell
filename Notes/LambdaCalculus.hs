module LambdaCalculus(

)
where
  import Data.Map

  type String = [Char]
  type Name = [Char]

  data Expr = Name |
              Lambda String Expr |
              Application Expr Expr

  type Environment = Map String Result
  type Result = (Expr, Environment)
  type Value = L

  -- Expr
  -- (Application (Lambda "x" (Lambda "y" Application "x" "y")) (Lambda "x" "x"))

  eval :: Expr -> Reader Environment Value
