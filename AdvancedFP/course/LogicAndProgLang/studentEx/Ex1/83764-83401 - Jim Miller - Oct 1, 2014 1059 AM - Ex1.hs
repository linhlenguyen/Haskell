-- Jim Miller
-- CS 510 Logic Programming
-- Exercise 1, basic algorithms for propositional logic in Haskell
-- Due 10/1/14

module HW1 where

import SimpleProp
import Data.List
import Data.Maybe

-- addvars removes duplicate variables and puts them in order.
addvars :: Ord a => Prop a -> [a] -> [a] 
addvars TruthP propVars = propVars
addvars AbsurdP propVars = propVars
addvars (LetterP x) propVars =  sort (nub (x:propVars))
addvars (NotP x) propVars =  sort (nub (addvars x propVars))
addvars (AndP x y) propVars =  sort (nub ((addvars x propVars) ++ (addvars y [])))
addvars (OrP x y) propVars =  sort (nub ((addvars x propVars) ++ (addvars y [])))
addvars (ImpliesP x y) propVars =  sort (nub ((addvars x propVars) ++ (addvars y [])))

vars :: Ord a => Prop a -> [a] 
vars p = addvars p []

assigns :: [Int] -> [[(Int, Bool)]]
assigns propVars = buildVals propVars [[]]
   where buildVals :: [Int] -> [[(Int, Bool)]] -> [[(Int, Bool)]]
         buildVals [] vals = vals
         buildVals propVars vals =
            buildVals (init propVars) ((map ([((last propVars), True)] ++) vals) ++
                                       (map ([((last propVars), False)] ++) vals))

taut :: Prop Int -> Bool
taut p = checkRemainingVals (allValuations p)
   where allValuations :: Prop Int -> [[(Int, Bool)]]
         allValuations p = assigns (vars p)
         checkRemainingVals :: [[(Int, Bool)]] -> Bool
         checkRemainingVals [] = True
         checkRemainingVals remainingVals =
            if (value vf p) then checkRemainingVals (tail remainingVals) else False
               where vf :: Int -> Bool
                     vf var = fromJust (lookup var (head (remainingVals)))

-- Two props are automatically unequal if they have different numbers of variables.
-- In my implementation, equality testing ignores different variable naming, but
-- order is important.  E.g.:
-- (P1 ~> P2) equals (P3 ~> P4), but (P1 ~> P2) does not equal (P2 ~> P1)
equal :: Prop Int -> Prop Int -> Bool
equal p q = if (length (vars p) /= length (vars q)) then False
   else checkRemainingVals (allValuations p) (allValuations q)
      where allValuations :: Prop Int -> [[(Int, Bool)]]
            allValuations p = assigns (vars p)
            checkRemainingVals :: [[(Int, Bool)]] -> [[(Int, Bool)]] -> Bool
            checkRemainingVals [] [] = True
            checkRemainingVals remainingValsP remainingValsQ =
               if ((value vfP p) == (value vfQ q))
                  then checkRemainingVals (tail remainingValsP) (tail remainingValsQ) else False
                     where vfP :: Int -> Bool
                           vfP var = fromJust (lookup var (head (remainingValsP)))
                           vfQ :: Int -> Bool
                           vfQ var = fromJust (lookup var (head (remainingValsQ)))

-- Test cases (for use in testing functions, all passed)
t1 :: Prop Int
t1 = (ImpliesP (LetterP 1) AbsurdP)
t2 :: Prop Int   -- Tautology
t2 = (ImpliesP AbsurdP (LetterP 2))
t3 :: Prop Int
t3 = (AndP (ImpliesP AbsurdP (LetterP 3)) (LetterP 10))
t4 :: Prop Int
t4 = (OrP (ImpliesP AbsurdP (LetterP 4)) (LetterP 11))
t5 :: Prop Int   -- t5 and t6 should be equal
t5 = (ImpliesP (LetterP 5) (LetterP 12))
t6 :: Prop Int
t6 = (OrP (NotP (LetterP 6)) (LetterP 13))
