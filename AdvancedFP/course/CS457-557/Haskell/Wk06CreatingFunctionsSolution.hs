module CreatingFunctions  where

import Char(ord)

-- 1 ----------------------------------------
-- Write an identity function that returns  
-- the value it is given as input

identity x = x


-- 2 -----------------------------------------
-- Write the average function
-- average (6.0, 10.0) ---> 8.0

average:: (Double,Double) -> Double
average (x,y) = (x+y)/2.0


-- 3 --------------------------------------
-- write the sign function that returnss
--  +1 if its argument is poisitve
--  -1 if its argument is negative
--   0 if its argument is zero
-- Use the by cases paradigm

sign x | x <0 = (-1)
       | x >0 = 1
       | x==0 = 0

-- 4 --------------------------------------
-- write the isEven function
-- isEven 1  ----> "1 is not even"
-- isEven 4  ----> "4 is even"
--
-- Hint use:   show 5  ----> "5"

isEven:: Integer -> String
isEven x | even x = show x ++ " is even"
         | odd x = show x ++ " is not even"


-- 5 ------------------------------------
-- write the len function
-- len [1,2]     ---> 2
-- len [1,5,7,3] ---> 4
-- len "adg"     ---> 3
-- Hint:   follow the transformation
-- [1,5,7,3] ---> [1,1,1,1]  use a comprehension
-- sum [2,4,5] ---> 11

len:: [a] -> Integer
len x = sum [ 1 | i <- x]




-- 6 --------------------------------------
-- write the "99 bottles of beer" song
-- which prints the words of the song as follows:
-- bottles 99 --->
-- 
-- 99 bottles of beer on the wall.
-- 99 bottles of beer!
-- take one down, pass it around,
-- 98 bottles of beer on the wall.
-- 
-- 98 bottles of beer on the wall.
-- 98 bottles of beer!
-- take one down, pass it around,
-- 97 bottles of beer on the wall.
-- 
-- 97 bottles of beer on the wall.
-- 97 bottles of beer!
-- take one down, pass it around,
-- 96 bottles of beer on the wall.
--
-- Hint use a comprehension, and the (++) and concat functions
-- "my name"++ "is Tim"            -->  "my nameis Tim"
-- concat ["abc","123"," ","OK?"]  --> "abc123 OK?"
-- think about how you wrote the isEven function above.

bottles:: Integer -> String
bottles n = concat [ f x ++ g x ++ take ++ f(x-1) ++ "\n" | x <- [n,n-1 ..0 ] ]
  where f x = show x ++ " bottles of beer on the wall.\n"
        g x = show x ++ " bottles of beer!\n"
        take = "take one down, pass it around,\n"
        

-- 7 ----------------------------------------
-- write the factorial function that multiplies
-- all the numbers less then n. Eg.
-- fact 1 --> 1
-- fact 2 --> 2
-- fact 3 --> 6
-- fact 4 --> 24
-- Hint: use the following scheme
-- 5 ---> [1,2,3,4,5] -> 120
-- The product function will be useful
-- product [1,4,6] ---> 1*4*6 --> 24

fact n = product [1 .. n]

-- 8 -------------------------------------
-- write the value function that turns a
-- string of digits into an integer
-- Use the following scheme
-- "256" -> ['2','5','6']  --> [50, 53, 54 ]   (use the ord function)
-- [ [50, 53, 54 ] --> [2,5,6]                  (use a comprhension)
-- "256" --> 3 --> [0,1,2] --> [2,1,0] --> [10^2,10^1,10^0] --> [100,10,1]
--  [2,5,6] and [100,10,1] --> [(2,100),(5,10),(6,1)]  (use zip)
--  [(2,100),(5,10),(6,1)] --> [200,50,6]  (use a comprhension)
--  [200,50,6] --> 256                     (use the sum function)

value:: String -> Int
value x = sum zs
   where xs = [ ord c - ord '0' | c <- x ]
         n = len x
         ys = [ 10 ^ i | i <- [n-1,n-2 .. 0]]
         zs = [ i*j | (i,j) <- zip xs ys]

