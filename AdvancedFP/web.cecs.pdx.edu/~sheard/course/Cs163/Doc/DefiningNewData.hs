module DefiningNewData where


data Day = Sun | Mon | Tue | Wed | Thu | Fri | Sat
   deriving (Eq,Show)

weekday:: Day -> Bool
weekday Sun = False
weekday Sat = False
weekday _ = True


-- Expressions that encode things like (3+4), (4-x), and (4 * (3+y))
data Expr 
  = Var String
  | Constant Int
  | Oper Expr String Expr
  deriving Show
e1 = Oper (Constant 3) "+" (Constant 4)  
e3 = Oper (Constant 4) "*" (Oper (Constant 3) "+" (Var "y"))

data Tree a 
   = Tip
   | Fork (Tree a) a (Tree a)
    deriving Show

countTip Tip = 1
countTip (Fork left v right) = countTip left + countTip right

{-
countVars :: Expr -> Int

depthExpr :: Expr -> Int

eval:: (String -> Int) -> Expr -> Int
-}

-- University Personell that includes two different kinds of
-- peoplw: Faculty ans Students

-- data Personel = ...

{-
advisees:: Personel -> [Personel]

classesTaken -> Personel -> [String]
-}



