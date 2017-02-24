plus :: Nat ~> Nat ~> Nat
{plus Z n} = n
{plus (S x) n} = S {plus x n}

-------------------------------------------------
plusZ :: Nat' n -> Equal {plus n Z} n
plusZ Z = Eq
plusZ (x@(S m)) = check Eq
  where theorem indHyp = plusZ m
  
plusS :: Nat' n -> Equal {plus n (S m)} (S{plus n m})
plusS Z  = Eq
plusS (S x)  = Eq
  where theorem ih = plusS x

plusCommutes :: Nat' n -> Nat' m -> Equal {plus n m} {plus m n}
plusCommutes Z m = Eq
   where theorem lemma = plusZ m
plusCommutes (S x) m = Eq
   where theorem plusS,
                 indHyp = plusCommutes x m

plusAssoc :: Nat' n -> Equal {plus {plus n b} c} {plus n {plus b c}}
plusAssoc Z = Eq
plusAssoc (S n) = Eq
  where theorem ih = plusAssoc n

plusNorm :: Nat' x -> Equal {plus x {plus y z}} {plus y {plus x z}}
plusNorm Z = Eq
plusNorm (S n) = Eq
  where theorem plusS, ih = plusNorm n

-------------------------------------------------------------
data Bit:: Nat ~> *0 where
  One :: Bit (S Z)
  Zero :: Bit Z
  
data Binary:: (Nat ~> *0) ~> Nat ~> Nat ~> *0 where
  Nil :: Binary bit Z Z
  Cons:: bit i -> Binary bit w n -> Binary bit (S w) {plus {plus n n} i}

------------------------------------------------------------

add3Bits:: (Bit i) -> (Bit j) -> (Bit k) -> Binary Bit #2 {plus {plus j k} i}
add3Bits Zero Zero Zero = Cons Zero (Cons Zero Nil)
add3Bits Zero Zero One  = Cons One  (Cons Zero Nil)
add3Bits Zero One  Zero = Cons One  (Cons Zero Nil)
add3Bits Zero One  One  = Cons Zero (Cons One  Nil)
add3Bits One  Zero Zero = Cons One  (Cons Zero Nil)
add3Bits One  Zero One  = Cons Zero (Cons One  Nil)
add3Bits One  One  Zero = Cons Zero (Cons One  Nil)
add3Bits One  One  One  = Cons One  (Cons One  Nil) 

add :: Bit c -> Binary Bit n i -> Binary Bit n j -> Binary Bit (S n) {plus {plus i j} c}
add c Nil Nil = Cons c Nil
add c (Cons x xs) (Cons y ys) = 
  case add3Bits c x y of
    (Cons bit (Cons c2 Nil)) -> check Cons bit (add c2 xs ys)
       where theorem plusCommutes, plusAssoc, plusNorm

---------------------------------------------------
-- Logical operators

and :: Nat ~> Nat ~> Nat
{and Z Z} = Z
{and Z (S n)} = Z
{and (S n) Z} = Z
{and (S n) (S n)} = S Z

or :: Nat ~> Nat ~> Nat
{or Z Z} = Z
{or Z (S n)} = S Z
{or (S n) Z} = S Z
{or (S n) (S n)} = S Z

xor :: Nat ~> Nat ~> Nat
{xor Z Z} = Z
{xor Z (S n)} = S Z
{xor (S n) Z} = S Z
{xor (S n) (S n)} = Z

----------------------------------------
-- theorems about logical operators

andZ1:: Bit a -> Equal {and a Z} Z
andZ1 Zero = Eq
andZ1 One = Eq

andZ2:: Bit a -> Equal {and Z a} Z
andZ2 Zero = Eq
andZ2 One = Eq

andOne2:: Bit a -> Equal {and a (S Z)} a
andOne2 Zero = Eq
andOne2 One = Eq

andOne1:: Bit a -> Equal {and (S Z) a} a
andOne1 Zero = Eq
andOne1 One = Eq

andAs :: Bit a -> Bit b -> Bit c -> Equal {and {and a b} c} {and a {and b c}}
andAs Zero Zero Zero = Eq
andAs Zero Zero One  = Eq
andAs Zero One  Zero = Eq
andAs Zero One  One  = Eq
andAs One  Zero Zero = Eq
andAs One  Zero One  = Eq
andAs One  One  Zero = Eq
andAs One  One  One  = Eq

andAssoc a b c = andAs(fromX a) (fromX b) (fromX c)

---------------------------------------------------
-- extended Bit

data BitX:: Nat ~> *0 where
  OneX :: BitX (S Z)
  ZeroX :: BitX Z
  And:: BitX i -> BitX j -> BitX {and i j}
  Or:: BitX i -> BitX j -> BitX {or i j}
  Xor:: BitX i -> BitX j -> BitX {xor i j}
  X:: Int -> BitX a
  And3:: BitX i -> BitX j -> BitX k -> BitX {and i {and j k}}

---------------------------------------------------------
-- from extended bit to bit

fromX :: BitX n -> Bit n
fromX OneX = One
fromX ZeroX = Zero
fromX (Or x y) = or' (fromX x) (fromX y)
fromX (And x y) = and' (fromX x) (fromX y)
fromX (Xor x y) = xor' (fromX x) (fromX y)
fromX (And3 x y z) = and' (fromX x) (and' (fromX y) (fromX z))

and' :: Bit i -> Bit j -> Bit {and i j}
and' Zero Zero = Zero
and' Zero One = Zero
and' One Zero = Zero
and' One One = One

or' :: Bit i -> Bit j -> Bit {or i j}
or' Zero Zero = Zero
or' Zero One  = One
or' One  Zero = One
or' One  One  = One

xor' :: Bit i -> Bit j -> Bit {xor i j}
xor' Zero Zero = Zero
xor' Zero One  = One
xor' One  Zero = One
xor' One  One  = Zero

--------------------------------------------------------

data Pair:: Nat ~> *0 where
 Pair:: BitX hi -> BitX lo -> Pair {plus {plus hi hi} lo}

logicHalf :: Bit i -> Bit j ->
         Equal {plus {plus {and i j} {and i j}} {xor i j}} {plus i j}
logicHalf Zero Zero = Eq
logicHalf Zero One  = Eq
logicHalf One  Zero = Eq
logicHalf One  One  = Eq
 
halfAd :: (BitX i,BitX j) -> Pair {plus i j}
halfAd (i,j) = Pair (And i j)(Xor i j)
  where theorem lemma = logicHalf (fromX i) (fromX j)

logic3 :: Bit i -> Bit j -> Bit k ->
          (Equal {plus {plus {or {and i j} {or {and i k} {and j k}}} 
                             {or {and i j} {or {and i k} {and j k}}}} 
                       {xor i {xor j k}}} 
                 {plus j {plus k i}})
logic3 Zero Zero Zero = Eq
logic3 Zero Zero One  = Eq
logic3 Zero One  Zero = Eq
logic3 Zero One  One  = Eq
logic3 One  Zero Zero = Eq
logic3 One  Zero One  = Eq
logic3 One  One  Zero = Eq
logic3 One  One  One  = Eq

addthree :: BitX i -> BitX j -> BitX k -> Pair {plus j {plus k i}}
addthree i j k = Pair (Or (And i j) (Or (And i k) (And j k)))
                          (Xor i (Xor j k))
  where theorem lemma = logic3 (fromX i) (fromX j) (fromX k)                           
  
addBits :: BitX c -> Binary BitX n i -> Binary BitX n j -> Binary BitX (S n) {plus {plus i j} c}
addBits c Nil Nil = Cons c Nil
addBits c (Cons x xs) (Cons y ys) = 
  case addthree c x y of
    (Pair c2 bit) -> Cons bit (addBits c2 xs ys)
       where theorem plusCommutes, plusAssoc, plusNorm


xs :: Binary BitX #2 {plus {plus a a} b}
xs = Cons (X 1) (Cons (X 2) Nil)  

ys :: Binary BitX #2 {plus {plus a a} b}
ys = Cons (X 3) (Cons (X 4) Nil)
carry = (X 5)

ans = addBits carry xs ys

-------------------------------------------------
-- staged generator

data BC:: Nat ~> *0 where
 BC :: Code(BitX n) -> BC n

addCode :: BC c -> Binary BC n i -> Binary BC n j -> 
          (Binary BC (S n) {plus {plus i j} c} -> Code ans) -> Code ans
addCode c Nil Nil cont = cont(Cons c Nil)
addCode (BC c) (Cons (BC x) xs) (Cons (BC y) ys) cont = 
  [| case addthree $c $x $y of
     (Pair c2 bit) -> 
         $(addCode (BC[|c2|]) xs ys (cont . (Cons (BC[|bit|]))) ) |]
 where theorem plusCommutes, plusAssoc, plusNorm

{- -- Need new rules for lets
addCode2 :: BC c -> Binary BC n i -> Binary BC n j -> 
            (Binary BC (S n) {plus {plus i j} c} -> Code ans) -> Code ans
addCode2 c Nil Nil cont = cont(Cons c Nil)
addCode2 (BC c) (Cons (BC x) xs) (Cons (BC y) ys) cont = 
  [| let (Pair c2 bit) = addthree $c $x $y 
     in $(addCode (BC[|c2|]) xs ys (cont . (Cons (BC[|bit|]))) ) |]
 where theorem plusCommutes, plusAssoc, plusNorm
-}


cons n = Cons (BC(lift (X n)))

c = BC(lift(X 0))
reg1 = cons 1 (cons 2 (cons 3 Nil))
reg2 = cons 4 (cons 5 (cons 6 Nil))

liftBinary :: Binary BC n i -> Code(Binary BitX n i)
liftBinary Nil = [|Nil|]
liftBinary (Cons (BC x) xs) = [| Cons $x $(liftBinary xs) |]

ex1 = addCode c reg1 reg2 liftBinary

{-  Some sample generated code

prompt> addCode c reg1 reg2 liftBinary
[| case (%addthree (X 0) (X 1) (X 4)) of
  (Pair dnma enma) ->
    case (%addthree dnma (X 2) (X 5)) of
      (Pair inma jnma) ->
        case (%addthree inma (X 3) (X 6)) of
          (Pair nnma onma) ->
            %Cons enma (%Cons jnma (%Cons onma (%Cons nnma %Nil))) |]
-}            

------------------------------------------------------
-- Optimizing Bits

data BitC:: *0 ~> Nat ~> *0 where
 OneC :: c -> BitC c (S Z)
 ZeroC:: c -> BitC c Z
 AndC:: c -> BitC c i -> BitC c j -> BitC c {and i j}
 OrC:: c -> BitC c i -> BitC c j -> BitC c {or i j}
 XorC:: c -> BitC c i -> BitC c j -> BitC c {xor i j}
 And3C:: c -> BitC c i -> BitC c j -> BitC c k -> BitC c {and i {and j k}} 
 XC:: c -> Int -> BitC c a

cost:: BitC c a -> c
cost (OneC c) = c
cost (ZeroC c) = c
cost (AndC c _ _) = c
cost (OrC c _ _) = c
cost (XorC c _ _) = c
cost (And3C c _ _ _) = c
cost (XC c n) = c

---------------------------------------------------------
-- from extended bit to bit

fromC :: BitC c n -> Bit n
fromC (OneC c) = One
fromC (ZeroC c) = Zero
fromC (OrC c x y) = or' (fromC x) (fromC y)
fromC (AndC c x y) = and' (fromC x) (fromC y)
fromC (XorC c x y) = xor' (fromC x) (fromC y)
fromC (And3C c x y z) = and' (fromC x) (and' (fromC y) (fromC z))

andAssocC a b c = andAs(fromC a) (fromC b) (fromC c)

--------------------------------------------------

max n m = if n >= m then n else m
comb (c1,d1) (c2,d2) c d = (c1+c2+c,d+max d1 d2)
dec (cost,depth) = (cost,depth - 1)
depth (c,d) = d 

----------------------------------------
-- Meaning preserving rules

r1:: BitC (Int,Int) n -> Maybe (BitC (Int,Int) n)
r1 (AndC _ x (ZeroC y)) = Just(ZeroC (0,0))
    where theorem l4 = andZ1 (fromC x)
r1 _ = Nothing


r2:: BitC (Int,Int) n -> Maybe (BitC (Int,Int) n)
r2 (AndC _ (ZeroC x) y) = Just(ZeroC(0,0))
   where theorem l5 = andZ2 (fromC y)
r2 _ = Nothing   


r3:: BitC (Int,Int) n -> Maybe (BitC (Int,Int) n)
r3 (AndC _ (OneC x) j) = Just j
   where theorem l6 = andOne1 (fromC j)
r3 _ = Nothing  


r4:: BitC (Int,Int) n -> Maybe (BitC (Int,Int) n)
r4 (AndC _ i (OneC y)) = Just i
   where theorem l7 = andOne2 (fromC i)     
r4 _ = Nothing  


r5:: BitC (Int,Int) n -> Maybe (BitC (Int,Int) n)
r5 (AndC _ (AndC x a b) (AndC y c d)) = Just(And3C (comb x y (2-1) 1) (AndC x a b) c d)
r5 _ = Nothing 


r6:: BitC (Int,Int) n -> Maybe(BitC (Int,Int) n)
r6 (AndC _ (AndC x a b) (AndC y c d)) = Just(And3C (comb x y (2-1) 1) a (AndC y b c) d)
   where theorem l2 = andAssocC b c d,
                 l3 = andAssocC a b (AndC x c d)
r6 _ = Nothing


r7:: BitC (Int,Int) n -> Maybe(BitC (Int,Int) n)
r7 (AndC _ (AndC x a b) (AndC y c d)) = Just(And3C (comb x y (2-1) 1) a b (AndC y c d))
   where theorem l3 = andAssocC a b (AndC x c d)
r7 _ = Nothing


r8:: BitC (Int,Int) n -> Maybe(BitC (Int,Int) n)
r8 (AndC _ a (AndC y b c)) = Just(And3C (comb (cost a) (dec y) (2-1) 1) a b c)
r8 _ = Nothing


r9:: BitC (Int,Int) n -> Maybe(BitC (Int,Int) n)
r9 (AndC _ (AndC x a b) c) = Just(And3C (comb (dec x) (cost c) (2-1) 1) a b c)
   where theorem l1 = andAssocC a b c 
r9 _ = Nothing

r10:: BitC (Int,Int) n -> Maybe(BitC (Int,Int) n)
r10 (And3C _ a b c) 
  | dc > testDepth =  Just(AndC (ca+cb+cc+2,dc) (AndC (1+ca+cb,testDepth) a b) c)
 where (ca,da) = cost a
       (cb,db) = cost b
       (cc,dc) = cost c
       testDepth = 1 + max da db
       theorem l8 = andAssocC a b c
r10 _ = Nothing 

------------------------------------------

rules :: [BitC (Int,Int) a -> Maybe (BitC (Int,Int) a)]
rules = [r1,r2,r3,r4,r5,r6,r7,r8,r9,r10]

first:: [BitC c n -> Maybe(BitC c n)] -> BitC c n -> Maybe(BitC c n)
first [] x = Nothing
first (t:ts) x = 
   case t x of
    Just y -> Just y
    Nothing -> first ts x

all:: [BitC c n -> Maybe(BitC c n)] -> BitC c n -> [BitC c n]
all [] x = [x]
all (f:fs) x = case f x of
                Nothing -> all fs x
                Just y -> y : all fs x

-------------------------------------------------
-- The list monad

listM = (Monad unit bind fail)
  where unit x = [x]
        fail s = []
        bind [] f = []
        bind (x:xs) f = f x ++ bind xs f   

monad listM

------------------------------------------------
-- The optimizer

cc x y = comb (cost x) (cost y) 1 1
cc3 x y z = comb (cost x) (comb (cost y) (cost z) 0 0) 2 1

g x = all rules x


retry rules [] x = x
retry rules (f:fs) x = 
  case f x of
    Nothing -> retry rules fs x
    Just y -> retry rules rules y

opt :: BitX n -> [BitC (Int,Int) n]
opt OneX = [OneC (0,0)]
opt ZeroX = [ZeroC (0,0)]
opt (And x y) = do { a <- opt x; b <- opt y; (g (AndC (cc a b) a b))}
opt (Or x y) = do { a <- opt x; b <- opt y; (g (OrC (cc a b) a b))}
opt (Xor x y) = do { a <- opt x; b <- opt y; (g (XorC (cc a b) a b))}
opt (And3 x y z) = 
  do { a <- opt x
     ; b <- opt y
     ; c <- opt z
     ; i <- g (And3C (cc3 a b c) a b c)
     ; return(retry [r10] [r10] i)}
opt (X n) = [XC (0,0) n]


test1 = And (X 0) (And (And (X 1) (X 2)) (And (X 2) (X 3))) 

test2 = And (X 0) (And (X 4) (And (X 1) (And (X 1) (And (X 2) (And (X 2) (X 3))))))


optC :: BitC (Int,Int) n -> [BitC (Int,Int) n]
optC (OneC _ ) = [OneC (0,0)]
optC (ZeroC _ ) = [ZeroC (0,0)]
optC (AndC _ x y) = 
   do { a <- optC x; b <- optC y; (all rules (AndC (cc a b) a b))}
optC (OrC _  x y) = 
   do { a <- optC x; b <- optC y; (all rules (OrC (cc a b) a b))}
optC (XorC _  x y) = 
  do { a <- optC x; b <- optC y; (all rules (XorC (cc a b) a b))}
optC (And3C _  x y z) = 
  do { a <- optC x
     ; b <- optC y
     ; c <- optC z
     ; all rules (And3C (cc3 a b c) a b c)}
optC (XC _ n) = [XC (0,0) n]

----------------------------------------------------

showB :: BitC c i -> Text    
showB (XC _ n) = Text ("x"++show n)
showB (AndC _  (XC _ n) (XC _ m)) = Text ("(And x"++show n++" x"++show m++")")
showB (OrC _ (XC _ n) (XC _ m)) =  Text ("(Or x"++show n++" x"++show m++")")
showB (XorC _ (XC _ n) (XC _ m)) = Text ("(Xor x"++show n++" x"++show m++")")
showB (And3C _ (XC _ n) (XC _ m) (XC _ p)) = Text ("(And3 x"++show n++" x"++show m++" x"++show p++")")
showB (AndC _ a b) = align "(And " (showB a `A` Nl `A` 
                                 showB b `A` Text ")")
showB (OrC _ a b)  = align "(Or "  (showB a `A` Nl `A` 
                                 showB b `A` Text ")")
showB (XorC _ a b) = align "(Xor " (showB a `A` Nl `A` 
                                 showB b `A` Text ")")
showB (And3C _ a b c) = align "(And3 " (showB a `A` Nl `A` 
                                     showB b `A` Nl `A` 
                                     showB c `A` Text ")")                                 


showA :: BitX i -> Text 
showA (X n) = Text ("x"++show n)
showA (And (X n) (X m)) = Text ("(And x"++show n++" x"++show m++")")
showA (Or (X n) (X m)) =  Text ("(Or x"++show n++" x"++show m++")")
showA (Xor (X n) (X m)) = Text ("(Xor x"++show n++" x"++show m++")")
showA (And3 (X n) (X m) (X p)) = Text ("(And3 x"++show n++" x"++show m++" x"++show p++")")
showA (And a b) = align "(And " (showA a `A` Nl `A` 
                                 showA b `A` Text ")")
showA (Or a b)  = align "(Or "  (showA a `A` Nl `A` 
                                 showA b `A` Text ")")
showA (Xor a b) = align "(Xor " (showA a `A` Nl `A` 
                                 showA b `A` Text ")")
showA (And3 a b c) = align "(And3 " (showA a `A` Nl `A` 
                                     showA b `A` Nl `A` 
                                     showA c `A` Text ")")                                 

showX x = do { putStr (render (showA x)); putStr "\n" }
  where monad ioM
                     
ioM = Monad returnIO bindIO failIO
                   
showc [] = putStr "\n"
showc (x:xs) = do { putStr (render (showB x))
                  ; putStr "\n"
                  ; showc xs
                  ; return ()}
  where monad ioM           
  
(Cons a1 (Cons a2 (Cons a3 _))) = ans  


-------------------------------------------------
-- Text documents

data Text:: *0 where
  Text :: String -> Text
  Nl :: Text
  Indent :: Int -> Text -> Text
  A :: Text -> Text -> Text

length [] = 0
length (x:xs) = 1 + length xs

align :: String -> Text -> Text
align s d = Indent (length s) (Text s `A` d)

replicate 0 c = []
replicate n c = c : replicate (n-1) c

render' :: Int -> Text -> String -> String
render' i (Text s) x = s ++ x
render' i Nl x = "\n" ++ replicate i ' ' ++ x
render' i (Indent j d) x = render' (i + j) d x
render' i (A d1 d2) x = render' i d1 (render' i d2 x )

render :: Text -> String
render d = render' 0 d ""