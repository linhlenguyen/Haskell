module Ex02ListLib where

import List
import Char(digitToInt)

-- Name of pair programmer 1 ___________________________

-- Name of pair programer 2  ___________________________


{-
The purpose of this assignment is to explore and use the functions
from the list library.

Explore the help page for the list library at the URL:
   http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-List.html

For each question, write the described function using the list library
functions listed for that question. You should consult the help pages for
what the library functions do.
-}

-- 1 ------------------------------------------------------------
-- Write the palindrome function that answers if two strings are
-- palindromes. Use the library function "reverse"

palindrome :: String -> String -> Bool
palindrome x y = x == undefined


-- 2 ----------------------------------------------------------------------------
-- write the functions that selects the longest even or odd prefix from a list.
-- evenPrefix [2,4,5,6,7] --> [2,4]
-- evenPrefix [3,25,6,7]  --> []
-- 
-- oddPrefix  [3,25,7,6] --> [3,25,7]
-- oddPrefix  [4,3,25,7,6] --> []

-- Use the libray functions "break", (and "fst" and "snd" from the prelude)

evenPrefix :: [Integer] -> [Integer]
evenPrefix x = undefined

oddPrefix :: [Integer] -> [Integer]
oddPrefix x = undefined

-- 3 ---------------------------------------------------------------------------------
-- write prefix which returns the longest prefix where all numbers are the same parity.
-- prefix [1,3,4,5,6] --> [1,3]  
-- prefix [2,4,6] --> [2,4,6]
-- use the library function "takeWhile" and case analysis

prefix :: [Integer] -> [Integer]
prefix x | null x = []
         | even (head x) = undefined
         | odd (head x) = undefined

-- 4 ----------------------------------------------------------
-- Write the function comma which behaves as follows:
-- comma 1243678 --> "1,243,678"
-- It turns an integer into a string, which places commas every three
-- digits, from right to left. Follow the design pattern given below.
--  1243678 --> "1243678" --> "8763421" -> ["8876","342","1"] -->
-- ["876", ",", "342", ",", "1"] --> "876,342,1" --> "1,243,678"
--
-- use the library functions "concat", "intersperse", and "reverse"
-- The function "group" that we defined in the programming out-loud
-- exercise in class is also useful. You can cut and paste it into
-- this file, but give it the name "groupN", since there is a function called
-- "group" in the List library.

-- cut and paste group here
groupN n =  undefined

comma:: Integer -> String
comma = undefined

-- 5 -------------------------------------------------------------
-- Create a function that add up all the digits in an Integer
-- For example:  addUp 28465  --> 2+8+4+6+5 = 25
-- Use the List library functions "map" and "sum", the "show"
-- function, and the Char library function "digitToInt" 

addUp:: Int -> Int
addUp x = undefined

-- 6 ----------------------------------------------------------
-- As in (5), except multiply all the digits
-- multUp 254 --> 2*5*4  = 40
-- Use the library function product

multUp :: Int -> Int
multUp x = undefined

-- 7 --------------------------------------------------------------
-- As in (5), and (6) except return the value of the largest digit
-- maxUp 254 --> 5
-- Use the library function maximum

maxUp :: Int -> Int
maxUp x = undefined

-- 8 -------------------------------------------------------------
-- Write a function that pairs up every element in a list with 
-- a given value.  For example
-- pairUp 5 [2,4,6,] --> [(2,5),(4,5),(6,5)]
-- pairUp True "abcd" --> [('a',True),('b',True),('c',True),('d',True)]
-- Use the library functions "repeat" and "zip"

pairUp :: a -> [b] -> [(b,a)]
pairUp item list = undefined


-- 9 --------------------------------------------------------
-- Write a function that determines if a String is the English 
-- name of a day of the week.
-- valid "Mon" --> False
-- valid "Monday" --> True
-- valid "Friday" --> True
-- valid "abc" --> False
--
-- Create a list of all valid day names and use the library function "elem"

valid :: String -> Bool
valid x = undefined
  where allDays =undefined

-- 10 ------------------------------------------------------
-- rewrite the function
-- test items = [ x+5 | x <- items, even x ]
-- but use the library functions "map" and "filter" rather than a comprehension
-- Ex02ListLib> test [1,2,3,4,5]   --> [7,9]

test :: [Integer] -> [Integer]
test x = undefined

