-- auxillary definitions

data BoolType :: *1 where
  T:: BoolType
  F:: BoolType
  
data Bool' :: BoolType ~> *0 where
  T:: Bool' T
  F:: Bool' F
  
---------------------------------------------
-- operations at the type level and their
-- singleton reflections at the value level

plus:: Nat ~> Nat ~> Nat
{plus Z m} = m
{plus (S n) m} = S {plus n m}

add:: Nat' i -> Nat' j -> Nat' {plus i j}
add 0v j = j
add (S x) j = S (add x j)

mult:: Nat ~> Nat ~> Nat
{mult Z m} = Z
{mult (S n) m} = {plus m {mult n m}}

multiply :: Nat' i -> Nat' j -> Nat' {mult i j}
multiply 0v j = 0v
multiply (S x) j = add j (multiply x j)

equalTo:: Nat ~> Nat ~> BoolType
{equalTo Z Z} = T
{equalTo Z (S n)} = F
{equalTo (S m) Z} = F
{equalTo (S m) (S n)} = {equalTo m n}

same:: Nat' i -> Nat' j -> Bool' {equalTo i j}
same Z Z = T
same Z (S n) = F
same (S m) Z = F
same (S m) (S n) = same m n

lessThan:: Nat ~> Nat ~> BoolType
{lessThan Z Z} = F
{lessThan Z (S n)} = T
{lessThan (S m) Z} = F
{lessThan (S m) (S n)} = {lessThan m n}

less:: Nat' i -> Nat' j -> Bool' {lessThan i j}
less Z Z = F
less Z (S n) = T
less (S m) Z = F
less (S m) (S n) = less m n


cond:: BoolType ~> Value ~> Value ~> Value
{cond T x y} = x
{cond F x y} = y

--------------------------------------------------------
-- A tagged sum type

data Value:: *1 where
  N:: Nat ~> Value
  B:: BoolType ~> Value

data Switch :: Value ~> Value ~> *0 where
  N:: Switch (N n) (N m)
  B:: Switch (B b) (B c)


-- The tree data type is indexed by the meaning of the tree

data Tr :: Value ~> *0 where
  Num :: Nat' i -> Tr (N i)
  Bool:: Bool' b -> Tr (B b)
  Add :: Tr (N i) -> Tr (N j) -> Tr (N {plus i j})
  Mult:: Tr (N i) -> Tr (N j) -> Tr (N {mult i j})
  Less:: Tr (N i) -> Tr (N j) -> Tr (B {lessThan i j})
  Equal:: Tr (N i) -> Tr (N j) -> Tr (B {equalTo i j})  
  Cond:: Tr (B b) -> Switch x y -> Tr x -> Tr y -> Tr {cond b x y}

t1 = (Cond (Bool T)
           N
           (Add (Num 1v) (Add (Num 2v) (Num 3v)))
           (Num 2v))
 

testN:: Bool' t -> Nat' i -> Nat' j -> {value {cond t (N i) (N j)}}
testN T x y = x
testN F x y = y

testB:: Bool' t -> Bool' i -> Bool' j -> {value {cond t (B i) (B j)}}
testB T x y = x
testB F x y = y

-- Evaluation reduces a tree to a value

value:: Value ~> *0
{value (N i)} = Nat' i
{value (B b)} = Bool' b

eval :: Tr n -> {value n}
eval (Bool T) = T
eval (Bool F) = F 
eval (Num i) = i
eval (Add t1 t2) = (eval t1) `add` (eval t2)
eval (Mult t1 t2) = (eval t1) `multiply` (eval t2)
eval (Less t1 t2) = (eval t1) `less` (eval t2)
eval (Equal t1 t2) = (eval t1) `same` (eval t2)
eval (Cond b N x y) = testN (eval b) (eval x) (eval y)
eval (Cond b B x y) = testB (eval b) (eval x) (eval y)
 

data StX :: *1 where
  NilSt :: StX
  ConsSt :: Value ~> StX ~> StX
    deriving List(x)

data Value' :: Value ~> *0 where
  N':: Nat' n -> Value' (N n)
  B':: Bool' b -> Value' (B b)

data Stack :: StX ~> *0 where
  Nil :: Stack NilSt
  Cons :: Value' v -> Stack ns -> Stack [v;ns]x
    deriving List(s)

s1 = [N' 3v,N' 5v, B' T]s


data Op :: StX ~> StX ~> *0 where
  Push :: Value' i -> Op s [ i;s]x
  Plus :: Op [N i,N j;s]x [N {plus i j};s]x
  Times :: Op [N i,N j;s]x [N {mult i j};s]x
  Eq :: Op [N i,N j;s]x [B {equalTo i j};s]x
  Lt :: Op [N i,N j;s]x [B {lessThan i j};s]x 
  IF:: Op [B i,t a,t b;s]x [{cond i (t a) (t b)};s]x


data Program :: StX ~> StX ~> *0 where
  NilP  :: Program s s
  ConsP :: Op s t -> Program t u -> Program s u
    deriving List(p)



p1 = [Push (N' 1v),Push (N' 1v),Plus]p
p2 = [Plus,Push (N' 1v),Push (N' 1v)]p
p3 = [Push (N' 1v),Push (N' 1v),Push (B' T),IF]p
    
    
compile :: Tr i -> Program [i;x]x y -> Program x y
compile (Num i)     acc = [Push (N' i);acc]p
compile (Bool i)     acc = [Push (B' i);acc]p
compile (Add t1 t2) acc = (compile t2 (compile t1 [Plus;acc]p))
compile (Mult t1 t2) acc = (compile t2 (compile t1 [Times;acc]p))
compile (Less t1 t2) acc = (compile t2 (compile t1 [Lt;acc]p))
compile (Equal t1 t2) acc = (compile t2 (compile t1 [Eq;acc]p))
compile (Cond t N x y) acc = 
        compile y (compile x (compile t [IF;acc]p))
compile (Cond t B x y) acc = 
        compile y (compile x (compile t [IF;acc]p))        


exec :: Program a b -> Stack a -> Stack b
exec [Plus  ;ops]p [N' i,N' j;s]s = exec ops [N' (add i j);s]s
exec [Times  ;ops]p [N' i,N' j;s]s = exec ops [N' (multiply i j);s]s
exec [Lt  ;ops]p [N' i,N' j;s]s = exec ops [B' (less i j);s]s
exec [Eq  ;ops]p [N' i,N' j;s]s = exec ops [B' (same i j);s]s
exec [IF; ops]p [B' T,x, y; s]s = exec ops [x;s]s
exec [IF; ops]p [B' F,x, y; s]s = exec ops [y;s]s
exec [Push i;ops]p      s   = exec ops [  i;s]s
exec []p                s  = s


gg:: Tr i -> Stack [i]x
gg t = exec (compile t NilP) Nil


{-
e1 = eval t1

-- In index type which describes stacks of natural numbers










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

-}

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
