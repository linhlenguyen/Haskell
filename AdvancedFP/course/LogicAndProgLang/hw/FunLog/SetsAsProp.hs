module SetsAsProp where

import Prop 

-------------------------------------------------
-- Thois module implements sets and subsets from
-- a universe of size N as a proposition. It works
-- by introducing Log (base 2) N  new propositional
-- variables, and each item in the universe is represented
-- as a formula over those variables. 

numbits:: Int -> Int
numbits n = ceiling (logBase 2 (fromIntegral n))

-- introduce some new variables to represent each of
-- the items from "xs", start at (LetterP i), and 
-- return a mapping from each item to its formula, also
-- teturn the next available index for a new LetterP
-- initial "abcd" 7   ----> 
--    ([('a',[~p7,~p8]),('b',[~p7,p8]),('c',[p7,~p8]),('d',[p7,p8])],9)

initial :: [a] -> Int -> ([(a, [Prop Int])], Int)
initial xs i = (zip xs (reverse (g i)),i+n)
  where n = numbits (length xs)
        g:: Int -> [[Prop Int]]
        g m | m > (n+i-1) = [[]]
        g n = map (LetterP n:) ys ++ map ((NotP (LetterP n)):) ys
           where ys =  (g (n+1))


-- given a universe (as a list) and some subset (as a list)
-- Order doesn't matter. Return a (Prop Int) that represents it.

subset :: Eq a => [a] -> [a] -> Prop Int
subset univ set = foldr acc AbsurdP univ
  where acc x prop | elem x set = orP (get x) prop
        acc x prop = prop
        (mapping,j) = initial univ 0
        get n = case lookup n mapping of
                  Just literals -> andL literals

item univ x = subset univ [x]

-- Given a universe and a set of directed edges bewteen
-- elements of the universe that represents a graph, return
-- a transition relatio as a (Prop n). This has two sets
-- of variables. The second set mirrors the first. Think
-- of it as a relation Trans(x,y,a,b)

trans univ pairs i = (fix,foldr acc AbsurdP pairs,j)
  where (mapping1,j) = initial univ i
        (mapping2,k) = initial univ j
        get n map = case lookup n map of
                  Just literals -> andL literals
                  Nothing -> error ("Cant find "++show n)
        acc (a,b) prop = orP (AndP (get a mapping1)  (get b mapping2)) prop  
        fix xs = andL (map rename (filter p xs))
        p (n,b) = n >= j
        rename (k,True) = LetterP(k-j)
        rename (k,False) = NotP (LetterP (k-j))

 