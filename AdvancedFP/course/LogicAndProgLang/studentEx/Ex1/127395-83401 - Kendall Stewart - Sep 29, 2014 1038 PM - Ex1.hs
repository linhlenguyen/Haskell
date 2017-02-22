{-
 - Kendall Stewart
 - CS510 Logic & Prog. Lang.
 - Exercise 1
 - 9.29.2014
 -}

module HW1 where

import SimpleProp
import Data.Maybe (fromJust)

-- "union" of ordered lists with unique values (similar to mergesort merge)
union      :: Ord a => [a] -> [a] -> [a]
union xs [] = xs
union [] ys = ys
union (x:xs) (y:ys) 
  | x == y  = x : union  xs    ys
  | x <  y  = x : union  xs   (y:ys)
  | x >  y  = y : union (x:xs) ys

-- synthesize variable list with union
addvars                  :: Ord a => Prop a -> [a] -> [a] 
addvars (LetterP  p)   xs = union [p] xs 
addvars (AndP     p q) xs = union (addvars p xs) (addvars q xs)
addvars (OrP      p q) xs = union (addvars p xs) (addvars q xs) 
addvars (ImpliesP p q) xs = union (addvars p xs) (addvars q xs) 
addvars (NotP     p)   xs = addvars p xs
addvars _              xs = xs

-- start with empty list
vars  :: Ord a => Prop a -> [a] 
vars prop = addvars prop [] 

-- crawl through search space
assigns       :: [Int] -> [[(Int,Bool)]]
assigns []     = [[]]
assigns (x:xs) = [ (x,b) : rest | rest <- assigns xs, b <- [True, False] ] 

-- check all possible valuations of the formula's variables
taut     :: Prop Int -> Bool
taut prop = and [ value (\t -> fromJust (lookup t ts)) prop | ts <- assigns (vars prop) ]

-- def'n of equivalence
equal    :: Prop Int -> Prop Int -> Bool
equal p q = taut $ (p ~> q) /\ (q ~> p)
