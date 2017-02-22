module Ex2 where

import SimpleProp

import qualified LazySmallCheck as SC
import LazySmallCheck
import Test.QuickCheck hiding(Prop(..))
import Test.HUnit
import Lecture2

-- implementation

simplify:: Ord n => Prop n -> Prop n
simplify (TruthP) = TruthP
simplify (AbsurdP) = AbsurdP
simplify (LetterP x) = LetterP x
simplify (NotP x) = NotP (simplify x)
simplify (AndP x y) = if (x == y) || (y == TruthP)
                            then (simplify x)
                       else if (x == TruthP)
                            then (simplify y)
                       else if (x == (NotP y) || y == (NotP x))
                           then AbsurdP
                       else (AndP (simplify x) (simplify y))
simplify (OrP x y) = if (x == y)
                            then (simplify x)
                       else if (y == TruthP)
                            then TruthP
                       else if (x == TruthP)
                            then TruthP
                       else if (x == (NotP y) || y == (NotP x))
                            then TruthP
                       else OrP (simplify x) (simplify y)
simplify (ImpliesP x y) = simplify
                           (OrP (NotP (simplify x)) (simplify y))

-- for testing

checkS x = and[ eval e x == eval e (simplify x) | e <- envs]

t1 = quickCheck checkS
t2 = smallCheck 3 checkS
