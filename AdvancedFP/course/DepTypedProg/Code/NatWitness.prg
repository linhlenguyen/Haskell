

data Boolean:: *1 where
  T:: Boolean
  F:: Boolean 
  
data Proof:: Boolean ~> *0 where
  Triv:: Proof T

data Plus:: Nat ~> Nat ~> Nat ~> *0 
      where
  PlusZ:: Plus Z m m
  PlusS:: Plus n m z -> 
          Plus (S n) m (S z)
          
data LE:: Nat ~> Nat ~> *0 where
   LeZ:: LE Z n
   LeS:: LE n m -> 
         LE (S n) (S m)

summandLessThanSum:: Plus a b c -> LE a c
summandLessThanSum (x@PlusZ) = LeZ
summandLessThanSum (PlusS x) =  (LeS (summandLessThanSum x))

gg:: Plus a b c -> LE b c




data Even:: Nat ~> *0 where
   EvenZ:: Even Z
   EvenSS:: Even n -> Even (S (S n)) 
   
e1 = EvenZ:: Even 0t

e2 = (EvenSS EvenZ) :: Even 2t


e3 = (EvenSS (EvenSS EvenZ)) :: Even 4t

p1 ::Plus 2t 3t 5t
p1 = PlusS (PlusS PlusZ)   

l1 = LeZ:: LE 0t a
l2 = (LeS LeZ):: LE 1t (1+a)t
l3 = (LeS (LeS LeZ)):: LE 2t (2+a)t

even :: Nat ~> Boolean
{even Z}     = T
{even (S Z)} = F
{even (S (S n))} = {even n}

even2 :: Proof {even 2t}
even2 = Triv

-- bad:: Proof {even 1t}
-- bad = Triv

data Seq :: Nat ~> *0 ~> *0 where
  Nil :: Seq Z a
  Cons :: a -> Seq n a -> Seq (S n) a
 deriving List(l)
  
length :: Seq n a -> Int  
length (x@Nil) = 0
length (y@(Cons x xs)) = 1 + length xs

leng :: Seq n a -> Nat' n 
leng (x@Nil) = Z
leng (y@(Cons x xs)) = (S (leng xs))

data N:: *0 where
  C:: Nat' n -> N
  
f:: Int -> N
f 0 = C Z
f n = C (S m)
  where (C m) = f (n-1)

