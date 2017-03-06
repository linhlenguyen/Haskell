-- Enter your code here. Read input from STDIN. Print output to STDOUT

data Op = Add | Sub | Mult | Div

data Expr = Value Int | Variable Char | Operator Op Expr Expr

data Token = TString | TBrackets Token

--10x + 2x - (3x + 6)/3
--(Operator Mult (Value 10) (Variable 'x')) --order of operation?
-- (-) lhs: ((+) lhs:(10x) rhs:(2x)) rhs: (/) (lhs: ((+) lhs:(3x) rhs:6) rhs:3)
--
--                  -
--          +               /
--      10x    2x       +       3
--                  3x     6
--
--                  +
--          10x             -
--                      2x      /
--                           +     3
--                         3x  6
--
--
{--Top level operators (anything that's not enclosed by brackets)
(1+(1+(1+(2x+3)))) + 5*(2+3+4*(x^2 -2))
                          +
      +
1         +

|--}

parseExp :: String -> [(Token,String)]
parseExp str = foldl foldf [] str
    where foldf :: [(Token,String)] -> Char -> [(Token,String)]
          foldf ((t:s):ts) c = undefined
