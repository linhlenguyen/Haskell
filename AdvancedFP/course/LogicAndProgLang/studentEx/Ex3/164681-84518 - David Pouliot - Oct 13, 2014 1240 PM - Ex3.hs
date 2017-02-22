-- Ex3 CS 510 - Logic Programming - David Pouliot

import SimpleProp


data Discrim a = Alpha a a | Beta a a | Lit a

discrim :: Prop a -> Discrim (Prop a)
discrim TruthP = Lit TruthP
discrim AbsurdP = Lit AbsurdP
discrim (LetterP s) = Lit (LetterP s)
discrim (AndP x y) = Alpha x y
discrim (OrP x y) = Beta x y
discrim (ImpliesP x y) = Beta (NotP x) y
discrim (NotP (OrP x y)) = Alpha (NotP x)  (NotP y)
discrim (NotP (ImpliesP x y)) = Alpha x (NotP y)
discrim (NotP (AndP x y)) = Beta (NotP x) (NotP y)
discrim (NotP (NotP x)) = discrim x
discrim (NotP TruthP) = Lit AbsurdP
discrim (NotP AbsurdP) = Lit TruthP
discrim (NotP (LetterP s)) = Lit (NotP (LetterP s))


p = LetterP "p"
q = LetterP "q"
r = LetterP "r"
s = LetterP "s"
t = LetterP "t"

-- 
tableau x = contcheck (prover [NotP x] [[(NotP x)]])

-- returns empty list if all paths have contradictions else returns list of paths with no contradictions
tableau2 x =  (prover2 [NotP x] [[(NotP x)]])
--
prover :: [Prop a] -> [[Prop a]] -> [[Prop a]]
prover [] paths = paths
prover (w:ws) paths = 
	case (discrim w) of
		-- the Lits get removed from first list	
		Lit x -> prover ws paths
		Alpha x y -> prover (x:y:ws) [x:y:ps | ps<- paths] 																							
		Beta x y -> prover (x:ws) [p:ps | p <- [x], ps <- paths] ++ prover (y:ws) [p:ps | p <- [y], ps <- paths]

-- modify to check for contradictions along the way instead of at the end
prover2 :: Eq a =>  [Prop a] -> [[Prop a]] -> [[Prop a]]
prover2 [] paths = paths
prover2 (w:ws) paths = 
	case (discrim w) of
		-- the Lits get removed from first list		
		Lit x -> prover2 ws paths
		Alpha x y -> prover2 (x:y:ws) [x:y:ps | ps<- paths, check2 (x:y:ps) == False] 																							
		Beta x y -> prover2 (x:ws) [p:ps | p <- [x], ps <- paths, check2 (x:ps) == False] ++ prover2 (y:ws) [p:ps | p <- [y], ps <- paths, check2 (y:ps) == False]


-- check for contradictions, ie p, NotP p - return True if all inner lists have a contradiction
contcheck ::Eq a => [[Prop a]] -> Bool
contcheck [[]] = True
contcheck [] = True
contcheck (x:xs)	
	| ((check2 x) == True) = contcheck xs
	| otherwise = False

-- check for contradictions, return True if there is a contradiction in the list
check2 :: Eq a => [Prop a] -> Bool
check2 [] = False
check2 (x:xs)
	| ((nots (NotP x)) `elem` xs) == True = True
	| otherwise = check2 xs 

-- helper to get rid of NotP (NotP x) 
nots :: Prop a -> Prop a
nots (NotP (NotP x)) = x
nots (NotP x) = NotP x

