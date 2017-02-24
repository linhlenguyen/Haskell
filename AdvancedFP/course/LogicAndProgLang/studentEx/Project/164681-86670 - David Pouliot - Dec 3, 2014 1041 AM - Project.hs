-- David Pouliot - CS510 Logic Programming - Project

module Project where

import ParseLine

import Data.List
import Data.List.Utils


-- input is a list of code lines
t1 = ["/* static_array_test */", "int main(int argc, char** argv)","{"
		,"int index;", 
		"if(index != 4)", "return -1;", "int some_array[4] = {1,2,3,4};",
		"int secret = some_array[index]; /* overflow */", "return 0;","}"]

t2 = ["/* static_array_test */", 
		"int main(int argc, char** argv)",
		"{",
		"int index;", 
		"int next;", 
		"index = 5 + 8;", 
		"next = index * 2;", 
		"}"]


t3 = ["/* static_array_test */", 
		"int main(int argc, char** argv)",
		"{",
		"int index;", 
		"int next;", 
		"next = index * 2;", 
		"}"]

test11 = run t1 []
test12 = run t2 []

test13 = writeY test12

test14 = reverse( "(check)":test12 )

test15 = runs t2

test16 = getObCheck test15 []

max = 2147483647
min = -2147483648

-- (assert (or (> index 2147483647) (< index -2147483647)))

t4 = writeY (runs t2)
t5 = writeY (runs t3)

-- end of tests, beginning of code
-- writeY to write to file		
writeY x = writeFile "test.ys" ( unlines x)	

-- runs to take input, list of c lines and outputs a list of yices commands
runs x = (reverse ( "(show-model)":"(check)":(getObCheck (run x []) []):(run x []) ))

-- run takes in the list of code lines and list of constraints
-- and returns the list of constraints to use in the solver
run [] constraints = constraints
run (x:xs) constraints 
	| "/*" `isPrefixOf` x  = run xs constraints  -- ignore comments
	| "\\" `isPrefixOf` x  = run xs constraints  -- ignore comments
	| "int main" `isPrefixOf` x  = run xs constraints  -- ignore main function for now
	| "{" `isPrefixOf` x  = run xs constraints  -- ignore 
	| "}" `isPrefixOf` x  = run xs constraints  -- ignore 
	| otherwise = run xs (parseLine x constraints) 

test33 = '=' `elem` "next = index - 2"

-- take in a line, parse it to get variable assignments, declarations, etc
--parseLine x constraints = getYices var constraints []
--parseLine:: [a] -> [a] -> [a]

parseLine x [] 
	| "int" `isPrefixOf` x = ("(define " ++ (replace ";" "" (replace "int " "" x))++"::int)"):[]		
	| '=' `elem` x = ("(assert " :(replace ";" "" (prefix x)):")":[])
	| otherwise = []
	
	
parseLine x (c:cs) 
	| "int" `isPrefixOf` x = ("(define " ++ (replace ";" "" (replace "int " "" x))++"::int)"):c:cs		
	| '=' `elem` x = ("(assert " ++(replace ";" ""((prefix x)++")"++[]))):c:cs
	| otherwise = (c:cs)



getObCheck [] [] = []
getObCheck [] check = "(assert (or " ++check++" ))"	
getObCheck (x:xs) check
		| "(define" `isPrefixOf` x = getObCheck xs (("(> "++(getVarName x)++" 2147483647)")++("(< "++(getVarName x)++" -2147483648)")++check)
		| otherwise = getObCheck xs check 
		
getVarName x = (replace "(define " "" ( replace "::int)" "" x))



 -- | "int" `isPrefixOf` x = (delete "int " x):c:cs	
-- 2 strategies here.   one is to split the line list of words, but this requires
-- whitespace as a seperator.   

{-
		
(define a::int)
(define b::int)
(define c::int)
(assert (= (+ a b c) 0))

-}
		
		
-- distinguish between and and or with the list of constraints, no should
-- all be ands.  Ors mean another list or tree branch

-- patterns
-- int name
-- int name = 
-- name = a + b

-- if(



-- store list of declared variables and types?


-- when see name, check if in list of declared variables, if it is, add 
-- constraint to contraint list


-- With if statement, branch into two lists of constraints or tree 

-- add keyword to make variable symbolic - don't need this

-- just do ints for now to keep it simple, no other types


-- add checks/constraints for memory overflows 

-- add constriants for divide by zero

-- add constraints to check for integer addition overflows
-- just add constraints less than largest int and greater than smallest





-- if there is a satisfying assignment, then there is an error
--getYices [] [] result = result
--getYices (v:vs) [] result = 
--getYices [] (c:cs) result = 
--getYices (v:vs) (c:cs) result = 

