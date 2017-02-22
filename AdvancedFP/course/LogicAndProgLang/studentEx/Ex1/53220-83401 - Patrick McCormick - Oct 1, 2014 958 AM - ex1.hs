-- Patrick McCormick
-- Excercise 1, cs 510 

import Data.List
import Control.Monad

import SimpleProp

addvars :: Ord a => Prop a -> [a] -> [a] 
addvars (TruthP) as = as
addvars (AbsurdP) as = as
addvars (LetterP a) as = a:as
addvars (NotP a) as = (addvars a as)++as
addvars (AndP x y) as = (addvars x as)++(addvars y as)++as
addvars (OrP x y) as = (addvars x as)++(addvars y as)++as
addvars (ImpliesP x y) as = (addvars x as)++(addvars y as)++as

vars :: Ord a => Prop a -> [a]
vars p = nub $ addvars p []

-- assigns :: [Int] -> [[(Int,Bool)]]
assigns l = z a b where
    a = repeat l
    b = replicateM (length l) [True, False]
    z (a:as) (b:bs) = (zip a b) : (z as bs)
    z [] _ = []
    z _ [] = []

-- helper fns
-- map pairs of (k,v) to k -> v
cf (v:vs) t = if (fst v) == t then snd v else cf vs t

-- take output from assigns and make it functions
fns (x:xs) = (cf x) : fns xs
fns [] = []

-- apply a list of functions to a Prop with value
apply p (fn:fns) = (value fn p) : apply p fns
apply p [] = []

-- map prop to truth tables
vals p = assigns $ vars p

-- taut:: Prop Int -> Bool
taut prop = all id (res fs) where
    vals = assigns $ vars prop
    funs (v:vs) = (cf v) : (funs vs)
    funs [] = []
    fs = funs vals
    res (fn:fns) = value fn prop : res fns
    res [] = []
    

-- equal:: Prop Int -> Prop Int -> Bool
equal p1 p2 = p1v == p2v where
    vals1 = vals p1
    vals2 = vals p2
    res1 = fns vals1
    res2 = fns vals2
    p1v = apply p1 res1
    p2v = apply p2 res2
