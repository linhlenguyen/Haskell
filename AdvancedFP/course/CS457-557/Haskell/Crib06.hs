module CreatingFunctions where

import List
import Array

--By equation
plus5 x = x + 5

last x = head(reverse x)

--By cases
absolute x | x < 0  = -x
           | x >= 0 = x

swap (x,y) | x < y = (x,y)
           | x > y = (y,x)
           | x==y  = (x,y)

--By patterns
myand True False = False
myand True True = True
myand False False = False
myand False True = False


--By local definition (where and let)
ordered = sortBy backwards [1,76,2,5,9,45]
  where backwards x y = compare y x
  
four = let double x = x+x
       in double 2
 
--By  use of a library
smallest = List.minimum [3,7,34,1]


--By lambda expression (anonymous functions)
descending = sortBy (\ x y -> compare y x) [1,76,2,5,9,45]

bySnd = groupBy (\ (x,y) (m,n) -> y==n) [(1,'a'),(3,'a'),(2,'c')]


--By parenthesizing binary operators
six:: Integer
six = foldr (+) 0 [1,2,3]  -- 1 + 2 + 3 + 0


--By section
add5ToAll = map (+5) [2,3,6,1]


--By currying  (partial application)

hasFour = any (==4)
doubleEach = map (\ x -> x+x)


--By composition

hasTwo = hasFour . doubleEach
empty = (==0) . length


--By combinator (higher order functions)
k x = \ y -> x  
all3s = map (k 3) [1,2,3]


--Using data and lookup (arrays  lists and finite functions)

whatDay x = ["Sun","Mon","Tue","Wed","Thu","Fri","Sat"] !! x

first9Primes = array (1,9) (zip [1..9] [2,3,5,7,11,13,17,19,23])
nthPrime x = first9Primes ! x
