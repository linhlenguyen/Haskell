module CribProgStruct (plus9,notSo) where
import List(length)
-- Integer examples

seven :: Integer
seven = 3 + 4

remainder1 = 10 `mod` 3

nine = (*) 3 3

{- Char and String -}

type Text = [Char]

firstAlpha = 'a'

myName =  first ++ " " ++ last
  where first = "John"
        last = "Smith"
        
-- Bool examples

notSo :: Bool
notSo = length "abc" == 4

theSame = "abc" == ['a','b','c']

justSo = notSo || theSame

-- List examples

upToTen:: [Integer]
upToTen = [1,2,3,4,5,6,7,8,9,10]

backwards = reverse upToTen

-- Tuples

twoTuple :: (Integer,String)
twoTuple = (34,"Hello World")

fourTuple = ('a',"X",True,1.2)

-- Double 

approxPi = 355.0 / 113.0 

-- Functions

plus9:: Integer -> Integer
plus9 x = x + 9

f:: a -> (a,a)
f x = (x,x)

palindromes x y = x == (reverse y)

-- Commands

read2Lines :: IO(String,String)
read2Lines = 
  do line1 <- getLine
     line2 <- getLine
     return(line1,line2)
        
                

main = putStrLn "Hello, World!"