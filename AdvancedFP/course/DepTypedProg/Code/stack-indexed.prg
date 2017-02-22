-- auxillary definitions

plus:: Nat ~> Nat ~> Nat
{plus Z m} = m
{plus (S n) m} = S {plus n m}

add:: Nat' i -> Nat' j -> Nat' {plus i j}
add 0v j = j
add (S x) j = S (add x j)

-- The tree data type is indexed by the meaning of the tree

data Tr :: Nat ~> *0 where
  Num :: Nat' i -> Tr i
  Add :: Tr i -> Tr j -> Tr {plus i j}

t1 = (Add (Num 1v) (Add (Num 2v) (Num 3v)))

-- Evaluation reduces a tree to a number

eval :: Tr n -> Nat' n
eval (Num i) = i
eval (Add t1 t2) = (eval t1) `add` (eval t2)

e1 = eval t1

-- In index type which describes stacks of natural numbers

data StX :: *1 where
  NilSt :: StX
  ConsSt :: Nat ~> StX ~> StX
    deriving List(x)

data Stack :: StX ~> *0 where
  Nil :: Stack NilSt
  Cons :: Nat' n -> Stack ns -> Stack [n;ns]x
    deriving List(s)

s1 = [3v,5v]s


data Op :: StX ~> StX ~> *0 where
  Push :: Nat' i -> Op s [i;s]x
  Plus :: Op [i,j;s]x [{plus i j};s]x

data Program :: StX ~> StX ~> *0 where
  NilP  :: Program s s
  ConsP :: Op s t -> Program t u -> Program s u
    deriving List(p)

p1 = [Push 1v,Push 1v,Plus]p
p2 = [Plus,Push 1v,Push 1v]p

-- I'd originally wanted compilation to proceed like:
-- compile :: Tr i -> Program x y
-- compile (Num i) = [Push i]
-- compile (Add t1 t2) = compile t1 ++ compile t2 ++ [Plus]
-- where singleton and append are replaced by appropriated indexed analogs
-- But Tim wanted to avoid dealing with the append function, and append laws by instead using an accumulating parameter p

compile :: Tr i -> Program [i;x]x y -> Program x y
compile (Num i)     acc = [Push i;acc]p
compile (Add t1 t2) acc = (compile t2 (compile t1 [Plus;acc]p))

c1 = compile t1 NilP

-- We didn't do the following in class, but it's all I need to finish the code:

exec :: Program a b -> Stack a -> Stack b
exec [Plus  ;ops]p [i,j;s]s = exec ops [add i j;s]s
exec [Push i;ops]p      s   = exec ops [  i;s]s
exec []p                s  = s

r1 = exec c1 Nil


{-
Extensions
 * multiplication
 * booleans
   ** if-then-else
      *** optimize so program doesn't evaluate both arms of the if-then-else, only evaluate the one that's needed.
 * big-sigma notation (this is half-baked)
   ** examples
      *** [sum from i=0 to 10 of i * i] would evaluate to 0+1+4+9+...+100 
   ** requires
      *** variables
      *** a second stack to hold current value of each sum index
-}
