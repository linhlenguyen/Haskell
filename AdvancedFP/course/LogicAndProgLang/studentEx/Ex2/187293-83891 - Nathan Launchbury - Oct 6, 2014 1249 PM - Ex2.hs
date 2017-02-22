module Ex2 where

import SimpleProp
import Ex1
import Lecture2
--import LazySmallCheck

-- Simplifies a proposition using very little code
-- Best used with a small amount of variables because of the expense of equiv
simplify1 prop = bottomUp equiv (cnf4 prop)

-- Does a better job, does more things
simplify2 prop = bottomUp equiv2 $ simplifyCNF $ cnf3 prop


-- Bottom up traversal which applies f
bottomUp :: (Prop a -> Prop a) -> Prop a -> Prop a
bottomUp f (AndP p q)     = f (AndP (bottomUp f p) (bottomUp f q))
bottomUp f (OrP p q)      = f (OrP (bottomUp f p) (bottomUp f q)) 
bottomUp f (ImpliesP p q) = f (ImpliesP (bottomUp f p) (bottomUp f q)) 
bottomUp f (NotP p)       = f (NotP (bottomUp f p))
bottomUp f p = f p

-- General equivalences (may be too general to be efficient) but currently outperforms equiv2 (in simplicity)
equiv :: Prop Int -> Prop Int
equiv p | numVars p >= 5                   = p
equiv (AndP p q)           | taut (p ~> q) = p
                           | taut (q ~> p) = q
equiv (OrP p q)            | equal p q     = p
equiv (ImpliesP p q)       | equal p q     = TruthP
equiv (ImpliesP p TruthP)                  = p
equiv p                                    = p

--numVars :: Prop a -> Int
numVars prop = length (vars prop)

-- Simple equivalences. Turns out this is terrible at actually simplifying things
equiv2 :: Prop Int -> Prop Int
equiv2 (AndP (LetterP u) (LetterP v))     | u == v = LetterP u
equiv2 (AndP p AbsurdP) = AbsurdP
equiv2 (AndP p TruthP) = p
equiv2 (AndP p q)                         | q == NotP q = AbsurdP
equiv2 (OrP (LetterP u) (LetterP v))      | u == v = LetterP u
equiv2 (OrP p TruthP) = TruthP
equiv2 (OrP p AbsurdP) = p
equiv2 (OrP p q)                          | q == NotP q = TruthP
equiv2 (ImpliesP (LetterP u) (LetterP v)) | u == v = TruthP
equiv2 (NotP (NotP p))                             = p
equiv2 p                                           = p

--undocnf :: [[Prop Int]] -> Prop Int
--undocnf 

-- Example props to simplify
example1 = ((p1 ~> p2) /\ ((p3 \/ p2) ~> p2))
example2 = (((p1 \/ p3) /\ p3) ~> example1) ~> (example1 /\ p2)
example3 = ((example2 /\ p3) ~> (p1 \/ (p2 ~> example1)))
example4 = example1 /\ (example2 ~> (example3 \/  example1))



simplifyCNF :: [[Prop Int]] -> Prop Int
simplifyCNF [] = TruthP
simplifyCNF ([]:xss) = AbsurdP
simplifyCNF cnf = AndP (pullOut p pss) (simplifyCNF qss)
  where p = selectVariable cnf
        pss = [ xs | xs <- cnf, elem p xs]
        qss = [ xs | xs <- cnf, not (elem p xs)]


selectVariable cnf = biggest (histogram (concat cnf))

-- assume prop is in every clause of cnf
pullOut :: Prop Int -> [[Prop Int]] -> Prop Int
pullOut prop cnf = if xss==([]:xss) then prop else OrP prop (simplifyCNF xss) 
   where xss = [ [x | x <- xs, x /= prop] | xs <- cnf]

simplifyCNF_Or :: [[Prop Int]] -> Prop Int
simplifyCNF_Or [] = AbsurdP
simplifyCNF_Or ([]:xss) = simplifyCNF_Or xss
simplifyCNF_Or cnf = simplifyCNF cnf


--increment n ys = take n ys ++ [1 + ys!!n] ++ drop (n+1) ys
histogram xs = histogram' [] xs

histogram' :: Eq k => [(k, Int)] -> [k] -> [(k, Int)]
histogram' index [] = index
histogram' index (x:xs) = histogram' (increment x index) xs

increment :: Eq k => k -> [(k,Int)] -> [(k,Int)]
increment p []                   = [(p, 1)]
increment p ((q,n):qns) | p == q = (q, (n+1)):qns
                        | True   = (q, n):increment p qns

biggest :: [(k, Int)] -> k
biggest ((a, b):abs) = biggest' a b abs

biggest' :: k -> Int -> [(k, Int)] -> k
biggest' p n [] = p
biggest' p n ((q, m):qms) = if n > m then biggest' p n qms else 
                            biggest' q m qms


