-- Ted Cooper
-- <theod@pdx.edu>
-- Exercise 1

module Ex1 where

import Data.List (permutations, sort)
import Data.Maybe (fromJust)

import SimpleProp


addvars:: Ord a => Prop a -> [a] -> [a] 
addvars (LetterP x)    = (x :)
addvars (AndP x y)     = addvars x . addvars y
addvars (OrP x y)      = addvars x . addvars y
addvars (ImpliesP x y) = addvars x . addvars y
addvars (NotP x)       = addvars x
addvars _              = id

vars:: Ord a => Prop a -> [a] 
vars = flip addvars []

assigns:: [Int] -> [[(Int,Bool)]]
assigns is = let n   = length is
                 f m = permutations 
                     $ replicate m True 
                    ++ replicate (n - m) False
                 ps  = concatMap f [0..n]
             in map (zip is) ps

vfs:: Prop Int -> [Int -> Bool]
vfs = map ((.) fromJust . flip lookup) . assigns . vars

taut:: Prop Int -> Bool
taut p = all (flip value p) . vfs $ p

equal :: Prop Int -> Prop Int -> Bool
equal p q = let vfs'  = vfs p
                fvs p = map (flip value p) $ vfs'
            in sort (vars p) == sort (vars q) && fvs p == fvs q

-- implication test:
-- > equal (ImpliesP (LetterP 1) (LetterP 2)) (OrP (LetterP 2) (NotP (LetterP 1)))
-- True
