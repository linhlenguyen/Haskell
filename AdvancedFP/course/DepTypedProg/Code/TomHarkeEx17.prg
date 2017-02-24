

-- import "exercise13.prg"
-- import "exercise14.prg"

data Seq :: *0 ~> Nat ~> *0 where
  Nil :: Seq a Z
  Cons :: a -> Seq a n -> Seq a (S n)
 deriving List(q)

data LE:: Nat ~> Nat ~> *0 where
   Base:: LE Z n
   Step:: LE n m -> 
         LE (S n) (S m)

-- first variant works, assuming 'q' is the label for the Seq type:

filter :: (a -> Bool) -> Seq a n -> exists m . (Nat' m, Seq a m)
filter p []q = Ex (Z, []q)
filter p [x;xs]q = 
  let
    rest = filter p xs
  in
  if p x 
    then case rest of
      Ex (m,tail) -> Ex (S m,[x;tail]q)
    else rest

-- second variant fails:
-- This is slightly refactored as it seemed more concise to pattern match
--    before the if-then-else


filter2 :: (a -> Bool) -> Seq a n -> exists m . (LE m n, Nat' m, Seq a m)
filter2 p []q = Ex (Base, Z, []q)
filter2 p [x;xs]q = 
  case filter2 p xs of
    Ex (le,m,tail) ->
      if p x 
        then Ex (Step le,S m,[x;tail]q)
        -- else check undefined
        else Ex (up m le,m,tail)
        

---------------------------------
-- Tim proved the following interesting Lemma
-- that seems to be useful.


up:: Nat' n -> LE n m -> LE n (S m)
up Z Base = Base
up (S m) (Step p) = Step(up m p)




