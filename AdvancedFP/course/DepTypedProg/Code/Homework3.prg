
data Plus:: Nat ~> Nat ~> Nat ~> *0 
      where
  PlusZ:: Plus Z m m
  PlusS:: Plus n m z -> 
          Plus (S n) m (S z)
 deriving Nat(p)        

data Seq :: Nat ~> *0 ~> *0 where
  Nil :: Seq Z a
  Cons :: a -> Seq n a -> Seq (S n) a
 deriving List(l)

data LE:: Nat ~> Nat ~> *0 where
   LeZ:: LE Z n
   LeS:: LE n m -> 
         LE (S n) (S m)
         
-----------------------------------------------------------

same:: Nat' n -> LE n n
predLE:: Nat' n -> LE n (S n)
trans:: LE a b -> LE b c -> LE a c
f:: Nat' b -> Plus b c -> LE b c
sameNat:: Nat' a -> Nat' b -> Maybe(Equal a b)
filter::  (a -> Bool) -> Seq a n -> exists m .(Nat' m,Seq a m)