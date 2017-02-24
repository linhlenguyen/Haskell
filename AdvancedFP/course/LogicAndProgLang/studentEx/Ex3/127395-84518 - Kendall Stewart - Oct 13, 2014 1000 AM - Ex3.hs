{-
 - Kendall Stewart
 - CS510 Logic & Prog Lang
 - Exercise 3
 - 10/12/2014
 -}


import SimpleProp
import qualified Ex1
import Test.QuickCheck hiding (Prop(..))
import Control.Monad(liftM, liftM2)

data Discrim a = Alpha a a | Beta a a | Lit a

discrim :: Prop a -> Discrim (Prop a)
discrim TruthP = Lit TruthP
discrim AbsurdP = Lit AbsurdP
discrim (LetterP s) = Lit (LetterP s)
discrim (AndP x y) = Alpha x y
discrim (OrP x y) = Beta x y
discrim (ImpliesP x y) = Beta (NotP x) y
discrim (NotP (OrP x y)) = Alpha (NotP x)  (NotP y)
discrim (NotP (ImpliesP x y)) = Alpha x (NotP y)
discrim (NotP (AndP x y)) = Beta (NotP x) (NotP y)
discrim (NotP (NotP x)) = discrim x
discrim (NotP TruthP) = Lit AbsurdP
discrim (NotP AbsurdP) = Lit TruthP
discrim (NotP (LetterP s)) = Lit (NotP (LetterP s))

--------------

taut :: Eq n => Prop n -> Bool
taut prop = not $ tableau [NotP prop] [NotP prop]

--------------

tableau :: Eq n => [Prop n] -> [Prop n] -> Bool
tableau _ [] = False
tableau [] _ = True
tableau (p:ps) path =
  case discrim p of
    Alpha x y -> tableau (x : y : ps) (elim (x : y : path))

    Beta  x y -> tableau (x : ps) (elim (x : path)) ||
                 tableau (y : ps) (elim (y : path))

    Lit   x   -> tableau ps (elim path)

  where elim path | contradiction path = []
                  | otherwise          = path

--------------

contradiction :: Eq n => [Prop n] -> Bool
contradiction path = let path' = map simplify path 
                     in  elem AbsurdP path' || conjugatePair path'

--------------

simplify :: Prop n -> Prop n
simplify prop = 
  case prop of
    NotP (NotP p) -> simplify p
    NotP TruthP   -> AbsurdP
    NotP AbsurdP  -> TruthP
    _             -> prop

--------------

conjugatePair :: Eq n => [Prop n] -> Bool
conjugatePair [] = False
conjugatePair [x] = False
conjugatePair (LetterP x : xs) = elem (NotP (LetterP x)) xs || conjugatePair xs
conjugatePair (NotP x : xs) = elem x xs || conjugatePair xs
conjugatePair (x:xs) = conjugatePair xs

--------------

-- testing with quickcheck
instance Arbitrary a => Arbitrary (Prop a) where
  -- coarbitrary = undefined
  arbitrary = oneof [return AbsurdP, return TruthP
                    , liftM LetterP arbitrary
                    , liftM NotP arbitrary
                    , liftM2 AndP arbitrary arbitrary
                    , liftM2 OrP arbitrary arbitrary
                    , liftM2 ImpliesP arbitrary arbitrary
                    ]

-- compare with truth-table checker
-- (comment out ImpliesP from arbitrary first or Ex1.taut gets stuck)
check x = taut x == Ex1.taut x

-- Abusing quickcheck to see what the checker might get stuck on
benchTrue  x = taut x == True  ==> taut x == True
benchFalse x = taut x == False ==> taut x == False

