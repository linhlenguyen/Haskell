import Test.QuickCheck hiding(Prop(..))
import qualified LazySmallCheck as SC
import LazySmallCheck

import SimpleProp
import Lecture2

-- Ideas about reductions: I think this should be directed by a
-- cost model which we could of course model after real hardware.
-- Otherwise we are just awash in transformations that keep the 
-- same order just exchange operators. Or what about translating from
-- a set of operations to another, like eliminating implies except we
-- specify a set of operators and strive for that. Anyways, very
-- interesting assignment.

-- Single level reduction functions, currently just reduce "order" vs
-- putting into equivalent forms with same number (or more) of ops.

r_and (AndP x y)
    | x == y = x
    | x == AbsurdP || y == AbsurdP = AbsurdP
    | x == TruthP = y
    | otherwise = (AndP x y)

r_or (OrP x y)
    | x == y = x
    | x == AbsurdP = y
    | y == AbsurdP = x
    | x == TruthP = TruthP
    | otherwise = (OrP x y)

r_not (NotP TruthP) = AbsurdP
r_not (NotP AbsurdP) = TruthP
r_not (NotP (NotP n)) = n
r_not n = n

-- A function to hold a collection of reduction functions.
-- This is redundant now but I want a way to choose sets of
-- reductions without having to create a new top-down applicator..

r_prop p = r_fn p
    where r_fn (AndP x y) = r_and (AndP x y)
	  r_fn (OrP x y) = r_or (OrP x y)
	  r_fn (NotP x) = r_not (NotP x)
	  r_fn n = id n

-- Given a reduction function and a Prop, return a reduced
-- Prop. top down and bottom up.

applyTD fn p = 
  case p of
    AbsurdP -> AbsurdP
    TruthP  -> TruthP
    (LetterP x) -> LetterP x
    (AndP x y) -> fn (AndP (applyTD fn x) (applyTD fn y))
    (OrP x y) -> fn (OrP (applyTD fn x) (applyTD fn y))
    (ImpliesP x y) -> fn (ImpliesP (applyTD fn x) (applyTD fn y))
    (NotP x) -> fn (NotP (applyTD fn x))

applyBU fn p =
  case p of 
    AbsurdP -> AbsurdP
    TruthP  -> TruthP
    (LetterP x) -> LetterP x
    (AndP x y) -> AndP (applyBU fn x) (applyBU fn y)
    (OrP x y) -> OrP (applyBU fn x) (applyBU fn y)
    (ImpliesP x y) -> ImpliesP (applyBU fn x) (applyBU fn y)
    (NotP x) -> applyBU fn x



-- ~~T /\ (p1 \/ p1) -> p1
ss = (AndP (NotP (NotP TruthP)) (OrP (LetterP 1) (LetterP 1)))

-- quickcheck stuff, caught an error, then I fixed it, now it seems to work!
checkTD x = and[ eval e x == eval e (applyTD r_prop x) | e <- envs]
checkBU x = and[ eval e x == eval e (applyTD r_prop x) | e <- envs]

t1 = quickCheck check
t2 = smallCheck 4 check

