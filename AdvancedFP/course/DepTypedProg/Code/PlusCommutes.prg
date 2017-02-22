
data Plus':: Nat ~> Nat ~> Nat ~> *0 where
  PlusZZ :: Plus' Z Z Z
  Succ1 :: Plus' a b c -> Plus' (S a) b (S c)
  Succ2 :: Plus' a b c -> Plus' a (S b) (S c)
   deriving syntax(m) Nat(PlusZZ,Succ1) Tick(Succ2)

data Color :: *1 where
  Red :: Color
  Blue:: Color
  
data Plus3:: Color ~> Nat ~> Nat ~> Nat ~> *0 where
  PlsZZ :: Plus3 Red Z Z Z
  Suc1 :: Plus3 Red a b c -> Plus3 Red (S a) b (S c)
  Suc2 :: Plus3 color a b c -> Plus3 Blue a (S b) (S c)
   

com2 :: Plus' a b c -> Plus' b a c
com2 PlusZZ = PlusZZ
com2 (a@(Succ1 n)) = Succ2 (com2 n)
com2 (a@(Succ2 n)) = Succ1 (com2 n)

com3:: Plus' a b c -> Plus a b c
com3 PlusZZ = PlusZ
com3 (a@(Succ1 n)) = PlusS (com3 n)
com3 (a@(Succ2 n)) = g (com3 n)

count1:: Plus' a b c -> Nat' a
count2:: Plus' a b c -> Nat' b
	
data Plus:: Nat ~> Nat ~> Nat ~> *0 
      where
  PlusZ:: Plus Z m m
  PlusS:: Plus n m z -> 
          Plus (S n) m (S z)
 deriving Nat(p)          
          
-- To prove plus commutes we'll need two lemmas
-- Note how they both say something about properties
-- of addition

-- Zero is the identity, in both the 1st (PlusZ:: Plus Z m m) 
-- and second (h:: Nat' n -> Plus n Z n) position of the Plus relation
-- Note we also need a Nat' argument to induct over.

h:: Nat' n -> Plus n Z n
h Z = PlusZ
h (p@(S n)) = PlusS(h n)

-- 'g' is an analog to 
--     PlusS:: Plus a b c -> Plus (S a) b     (S c)
--     g    :: Plus a b c -> Plus a     (S b) (S c)
-- But i different positions.

g:: Plus a b c -> Plus a (S b) (S c)
g PlusZ = PlusZ
g (p@(PlusS x)) = PlusS(g x)


commute:: Nat' b -> Plus a b c -> Plus b a c
commute n (p@PlusZ) = h n
commute n (p@(PlusS x)) = g(commute n x)

p1 :: Plus 3t 4t 7t
p1 = 3p


------------------------------------------------
-- We discussed last time how multiplication might be 
-- used in the type of a function.

data Seq :: Nat ~> *0 ~> *0 where
  Nil :: Seq Z a
  Cons :: a -> Seq n a -> Seq (S n) a
 deriving List(l)
 
spitOut:: Nat' m -> Seq m Int
spitOut Z = Nil
spitOut (S m) = Cons 0 (spitOut m)

-----------------------------------------------

plus :: Nat ~> Nat ~> Nat
{plus Z m} = m
{plus (S n) m} = S {plus n m}

app:: Seq n a -> Seq m a -> Seq {plus n m} a
app Nil ys = ys
app (Cons x xs) ys = Cons x (app xs ys)

-----------------------------------------------------

mult :: Nat ~> Nat ~> Nat
{mult Z m} = Z
{mult (S n) m} = {plus m {mult n m}}

many :: Nat' n -> Nat' m  -> Seq {mult n m} Int
many Z x = Nil
many (S m) xs = app (spitOut xs) (many m xs)
 
               


-----------------
data Rep :: *0 ~> *0 where
  Int :: Rep Int
  Char :: Rep Char
  Prod:: Rep a -> Rep b -> Rep (a,b)
  List:: Rep a -> Rep [a]
  

repEQ :: Rep a -> Rep b -> Maybe(Equal a b)
repEQ Int Int = Just Eq
repEQ Char Char = Just Eq
repEQ (l1@(List x)) (l2@(List y)) =
   case repEQ x y of
     Nothing -> Nothing
     Just(p@Eq) -> Just Eq
repEQ (Prod a b) (Prod m n) = 
  case (repEQ a m,repEQ b n) of
    (Just Eq,Just Eq) -> Just Eq
    _ -> Nothing
repEQ _ _ = Nothing