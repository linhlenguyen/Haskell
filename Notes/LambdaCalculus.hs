module LambdaCalculus(

)
where
  import Data.Map

  type Name = [Char]

  data Expr = Name |
              Lambda Name Expr |
              Application Expr Expr

  type Environment = Map Name Result
  type Result = (Expr, Environment)
  type Value = L Name Result

  -- Expr
  -- (Application (Lambda "x" (Lambda "y" Application "x" "y")) (Lambda "x" "x"))

  eval :: Expr -> Reader Environment Value
