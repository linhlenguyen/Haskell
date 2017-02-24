import "Nat.html"

data Index :: *1 where
  I:: Index
  O:: Index

value:: Index ~> Nat
{value I} = S Z
{value O} = Z



data Bit:: Index ~> *0 where
  I:: Bit I
  O:: Bit O

valueR:: Bit i -> Nat' {value i}
valueR I = 1v
valueR O = 0v

data Seq :: *1 ~> *1 where
  SNil:: Seq a
  SCons:: a ~> Seq a ~> Seq a
 deriving List(s)
 
data Mem:: Nat ~> Seq Index ~> *0 where 
  MNil :: Mem Z SNil
  MCons :: Bit i -> Mem n s -> Mem (S n) (SCons i s)
 deriving List(m)

data Addr:: Nat ~> Nat ~> *0 where
  ANil:: Addr Z Z
  ACons:: Bit i -> Addr m v -> Addr (S m) {plus {value i} {plus v v}}
 deriving List(a)

split:: Nat' i -> Plus i j k -> 
        Addr k x -> exists v u .
        (Addr i v,Addr j u,Equal {plus v {shift i u}} x)

data Tags:: Nat ~> Nat ~> Seq Nat ~> *0 where
  TNil:: Tags Z i SNil
  TCons:: Addr i v -> Tags n i vs -> Tags (S n) i (SCons v vs)
 deriving List(t)


data Cache:: Seq Index ~> *0 where
 Cache:: Mem 4t xs ->     -- Cache of size 4
         Tags 4t 1t ws -> -- Tag vector
         Tags 4t 1t vs -> -- Validity bits
         Mem 8t ys ->     -- Memory, may not be up to date
         OK xs ws vs ys zs -> 
         Cache zs

data Boolean :: *1 where
  T :: Boolean
  F :: Boolean
  
data Proof :: Boolean ~> *0 where
  P:: Proof T

data OK:: Seq Index ~> Seq Nat ~> Seq Nat ~> Seq Index ~>Seq Index ~> *0 where
  Invalid:: OK a b [Z,Z,Z,Z]s ys ys
  Evict :: Addr 1t i -> 
           Addr 2t j -> 
           Shift i j k ->   -- Equal {plus {shift 2t i} j} k
           NotEq i {get j tag}
           Eq {get j valid} 1t
           OK cache tag valid mem ans -> 
           OK {set j {get k mem} cache}
              {flip  j tag} 
              valid 
              {set {plus j {shift 2t {get j tag}}}
                   {get j cache} 
                   mem} 
              ans

get:: Nat ~> Seq a ~> a
{get Z (SCons x xs)} = x
{get (S n) (SCons x xs)} = {get n xs}

set:: Nat ~> a ~> Seq a ~> Seq a
{set Z y (SCons x xs)} = SCons y xs
{set (S n) y (SCons x xs)} = SCons x {set n y xs}

writeC:: Addr 3t i -> Bit x -> Cache xs -> Cache {set i x xs}

read:: Addr 3t i -> Mem 8t ys -> Bit {get i ys} 
read [O,O,O]a [a,b,c,d,e,f,g,h]m = a
read [I,O,O]a [a,b,c,d,e,f,g,h]m = b
read [O,I,O]a [a,b,c,d,e,f,g,h]m = c
read [I,I,O]a [a,b,c,d,e,f,g,h]m = d
read [O,O,I]a [a,b,c,d,e,f,g,h]m = e
read [I,O,I]a [a,b,c,d,e,f,g,h]m = f
read [O,I,I]a [a,b,c,d,e,f,g,h]m = g
read [I,I,I]a [a,b,c,d,e,f,g,h]m = h

write:: Addr 3t i -> Bit x -> Mem 8t xs -> Mem 8t {set i x xs}
write [O,O,O]a x [a,b,c,d,e,f,g,h]m = [x,b,c,d,e,f,g,h]m
write [I,O,O]a x [a,b,c,d,e,f,g,h]m = [a,x,c,d,e,f,g,h]m
write [O,I,O]a x [a,b,c,d,e,f,g,h]m = [a,b,x,d,e,f,g,h]m
write [I,I,O]a x [a,b,c,d,e,f,g,h]m = [a,b,c,x,e,f,g,h]m
write [O,O,I]a x [a,b,c,d,e,f,g,h]m = [a,b,c,d,x,f,g,h]m
write [I,O,I]a x [a,b,c,d,e,f,g,h]m = [a,b,c,d,e,x,g,h]m
write [O,I,I]a x [a,b,c,d,e,f,g,h]m = [a,b,c,d,e,f,x,h]m
write [I,I,I]a x [a,b,c,d,e,f,g,h]m = [a,b,c,d,e,f,g,x]m 
 