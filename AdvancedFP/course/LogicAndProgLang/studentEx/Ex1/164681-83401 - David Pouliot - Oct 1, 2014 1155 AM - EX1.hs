-- EX1 CS510 Logic & Programming - David Pouliot
--  Note there are bugs in this code.  I am new to Haskell and am still
-- learning its syntax.    I can see the recursive solution to most 
-- of these, but am not sure how to make it work with Haskell.   I will
-- catch on soon enough.

import SimpleProp

addvars :: Ord a => Prop a -> [a] -> [a] 
addvars a b 
	| a == LetterP x = x:b
	| a == AndP x y = addvars x [] : addvars y [] : b
	| a == OrP (Prop x) (Prop y) = addvars (Prop x) [] : addvars (Prop y) [] : b
	| a == ImpliesP (Prop x) (Prop y) = addvars (Prop x) [] : addvars (Prop y) [] : b
	| a == NotP (Prop x) = addvars (Prop x) b 
	| a == AbsurdP = b
	| a == TruthP = b

vars :: Ord a => Prop a -> [a]
vars a = addvars a []


assigns :: Int a => [a] -> 
assigns (x:s) 
	| s == [] = [(x, True), (x False)]
	| assignshelp (x, True) s []: assignshelp (x, False) s []
	
assignshelp (x, bool) (h:s) result
	| s == [] = (x, bool) : result
	| assignshelp (h, True) s (x, bool):result : assignshelp (h, False) s (x, bool):result 
	
taut:: Prop Int -> Bool
taut a
	tauthelp (assigns a) a
	
tauthelp (x:xs) a
	| value x a == False = False
	| xs == [] = value x a
	| tauthelp xs a
	
equal:: Prop Int -> Prop Int -> Bool
equal a b
	equalhelp (assigns a) a (assigns b) b
	
equalhelp (x:xs) a (n:ns) b
	| value x a == value n b = equalhelp xs a ns b
	| xs == [] && ns == [] = True
	| otherwise False



