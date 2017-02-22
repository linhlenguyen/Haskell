module HW1 where

import SimpleProp
import Data.List

addvars:: Ord a => Prop a -> [a] -> [a]

addvars (TruthP) a = a
addvars (AbsurdP) a = a
addvars (NotP x) a = addvars x a
addvars (AndP x y) a = addvars x (addvars y a)
addvars (OrP x y) a = addvars x (addvars y a)
addvars (ImpliesP x y) a = addvars x (addvars y a)
addvars (LetterP x) a = sort (union [x] a)

vars:: Ord a => Prop a -> [a]
vars p = addvars p []

assigns:: [Int] -> [[(Int,Bool)]]
assigns [] = []
assigns [x] = [[(x, True)], [(x, False)]]
assigns (x:xs) = union (map ((x, True) :) (assigns xs)) (map ((x, False) :) (assigns xs))

getValue a y = case lookup y a of
  Just z -> z
  Nothing -> False

isTruth x [] = value (getValue []) x
isTruth x [a] = value (getValue a) x
isTruth x (a:as) = (isTruth x [a]) && (isTruth x as)

taut:: Prop Int -> Bool
taut x = isTruth x (assigns (vars x))

isEqual x y [] = (value (getValue []) x) == (value (getValue []) y)
isEqual x y [a] = (value (getValue a) x) == (value (getValue a) y)
isEqual x y (a:as) = (isEqual x y [a]) && (isEqual x y as)

equal :: Prop Int -> Prop Int -> Bool
equal x y = isEqual x y (assigns (union (vars x) (vars y)))
