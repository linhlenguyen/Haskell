-- :: ~ has type
-- => type constraint
-- some types Eq, Ord, Num
-- Eq a => a is of comparable type
-- multiple contrainsts?

myFunc :: Integer -> Integer

-- Type classes
-- function combination
-- f(g(h x)) -> f.g.h x

Array
Some array definition [1,2,3,4], [2,4..24], [2,4], [20,19..1], [[],[],[]]
	
 + concatenation [1,2,3] + [2,4,5] //Append
 : prepend 1 : [2,3]
 [1,2,3] is syntactic sugar for 1:2:3:[]
 
 [x|x<-1,1.1..10,predicates,predicates]
 [y|y<-1,2...10]
 
 abgx360
 
 Ord::
 
 digitToInt :: Char -> Int
 digitToInt c
 | isDigit c            =  ord c - ord '0'
 | c >= 'a' && c <= 'f' =  ord c - ord 'a' + 10
 | c >= 'A' && c <= 'F' =  ord c - ord 'A' + 10
 | otherwise            =  error ("Char.digitToInt: not a digit " ++ show c)
 
 