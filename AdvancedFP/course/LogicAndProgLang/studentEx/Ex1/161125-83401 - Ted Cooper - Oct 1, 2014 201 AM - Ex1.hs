-- Ted Cooper
-- <theod@pdx.edu>
-- Exercise 1

module Ex1 where

import Control.Applicative ((<*>), pure)
import Data.List (permutations, sort, transpose)
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

assigns is = map (zip is) $ bns is

bns is = let n = length is 
         in take (2 ^ n) . transpose . take n . scanl f (cycle [True,False]) $ is
  where f acc r     = slow acc
        slow (x:xs) = x : x : slow xs

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
