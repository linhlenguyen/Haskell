--PalindromeModule

module PalindromeModule(
	isPalindrome,
	toNumber,
	firstElemDupCheck,
	hasNoDuplication,
	getMax
) where

--isPalindrome:: Integral -> Integer
--IsPalindrome
--Simple cases 
--1 character -> true
--2 characters -> s[0] == s[length-1]  
--3 recursive..

isPalindrome [] = False
isPalindrome (x:[]) = True
isPalindrome (s:x:[]) = s == x
--isPalindrome (x:xs) = if x == last xs then isPalindrome (take ((length xs) - 1) xs) else False
isPalindrome xs = if head xs == last xs then isPalindrome ((init.tail) xs) else False

--parallelise this!?
--break into substrings rather than recursion

--bool isPalindrome (string c){
--	int i, j;
--	i = 0, j = c.length -1
--	while (i != j){
--		if (c[i] != c[j])
--			return false
--		i++;j--;
--	}
--	return true;
--}

--Tuples
--let myTriangle = [(x,y,z)| x<-[1..10], y<--[1..10], z<--[1..10]]
--[variables | predicates]
--let x = [ x*y+z/3 | x = 1, y = 3...]

toNumber (x:[]) = x
toNumber xs = last xs + 10 * (toNumber (init xs))

firstElemDupCheck [] = False
firstElemDupCheck (x:y:[]) = if x == y then False else True
firstElemDupCheck (x:xs) = if head xs == x then False else firstElemDupCheck(x:(tail xs))

hasNoDuplication xs = if firstElemDupCheck xs then firstElemDupCheck(tail xs) else False

--Reverse
--reverse [] = []
--reverse (x:xs) = reverse xs ++ [x]

--Maximum
--max (x:xs)
-- | x > maxTail = x
-- | otherwise = maxTail
--where maxTail = max xs

getMax [x] = x
getMax (x:xs) = max x (getMax xs)

--Base conversion
--Conversion of a number n base n to base m
--Validitiy checking

--Number to list and list to number
--nToL Integer -> 
nToL 0 = []
nToL x = mod (x 10) : nToL div(x 10)


