{-
An attempt to do the compilation-to-stack-language example.

Source are expressions.
Expressions have a notion of evaluation.

-}

data Tr :: *0 where
  Num :: Int -> Tr
  Add :: Tr -> Tr -> Tr

t1 = (Add (Num 1) (Add (Num 2) (Num 3)))

eval :: Tr -> Int
eval (Num i) = i
eval (Add t1 t2) = (eval t1) + (eval t2)

e1 = eval t1

data Op :: *0 where
  Push :: Int -> Op
  Plus :: Op

compile :: Tr -> [Op]
compile (Num i) = [Push i]
compile (Add t1 t2) = compile t1 ++ compile t2 ++ [Plus]

c1 = compile t1

exec :: [Op] -> [Int] -> [Int]
exec (Plus  :ops) (i:j:s) = exec ops ((i+j):s)
exec (Push i:ops)      s  = exec ops (    i:s)
exec []                s  = s

x1 = exec c1 []

-- want:
--   eval t = head $ exec (compile t) []
-- which follows from the more general:
--   (eval t):init = exec (compile t) init

{-
Possible indexing
 * value of term
   ** but what of more complex languages
      *** dep. on I/O
      *** other effects
      *** variables
 * presence of enough data on top of stack
 * change in height of stack
 * (non-negativity of stack -- redundant)
 * stack programs are well-formed (ie they never exhaust the stack)
-}
