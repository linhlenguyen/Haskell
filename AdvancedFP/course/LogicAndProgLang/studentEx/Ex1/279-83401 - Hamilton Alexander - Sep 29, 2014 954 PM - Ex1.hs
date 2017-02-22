-- H Forrest Alexander: ML&PL Exercise 1

module HW1 where
import Data.Maybe(fromJust)
import Data.List(sort)
import SimpleProp

addvars :: Ord a => Prop a -> [a] -> [a] 
addvars p xs | p == AbsurdP || p == TruthP = []
addvars (LetterP y) xs = if y `elem` xs then xs else y:xs
addvars (NotP p) xs = addvars p xs
addvars p xs = foldr ($) xs $ map addvars 
  $ case p of AndP x y -> [x,y]; OrP x y -> [x,y]; ImpliesP x y -> [x,y]

vars:: Ord a => Prop a -> [a] 
vars = sort . flip addvars []

assigns:: [Int] -> [[(Int,Bool)]]
assigns [] = [[]]
assigns (x:xs) = concat [map ((x,b):) (assigns xs) | b <- [True, False]]

taut :: Prop Int -> Bool
taut = and . truthtab

equal :: Prop Int -> Prop Int -> Bool
equal a b = truthtab (a `withvars` b) == truthtab (b `withvars` a) 

evaluate p xs = value (fromJust . flip lookup xs) p
truthtab p = map (evaluate p) (assigns $ vars p)
a `withvars` b = foldr OrP a $ map ((\v -> AndP v $ NotP v) . LetterP) (vars b)

absurd = not . or . truthtab
