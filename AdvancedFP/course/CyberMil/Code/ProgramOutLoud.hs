module ProgramOutLoud where

import List(intersperse,elemIndices)
import Excell

-- Two problems
-- 1) group 3 [1,2,3,2,3,4,5,6,7,8]  --> [[1,2,3],[2,3,4],[5,6,7],[8]]
-- 2) comma 12478426                 --> "12,478,426"


-- Step 1 write down some charateristic input


x = [1,2,3,2,3,4,5,6,7,8]

-- Step 2 work through a sequence of steps, namimng each intermediate

y = iterate (drop 3) x
z = takeWhile (not . null) y
w = map (take 3) z

-- Step 3 Rearrange the steps as local definitiojns in a function

group0 x = w
  where y = iterate (drop 3) x
        z = takeWhile (not . null) y
        w = map (take 3) z

-- Step 4 Generalize any constants to new arguments

group1 n x = w
  where y = iterate (drop n) x
        z = takeWhile (not . null) y
        w = map (take n) z    

-- Step 5 Can you make it shorter? Simpler? better?

group n =  map (take n) . takeWhile (not . null) . iterate (drop n)
 
---------------------------------------

days = ["Sun","Mon","Tue","Wed","Thu","Fri","Sat"]

day = "Wed"
n = 30

[count] = elemIndices day days
i = 7 - count
dates = [1..n]
firstWeek = take i dates
restWeeks = group 7 (drop i dates)
cal = stack (blankRow count `beside` row firstWeek : map row restWeeks)

month :: String -> (String,Int) -> Table
month day (name,n) = cal
  where [count] = elemIndices day days
        i = 7 - count
        dates = [1..n]
        firstWeek = take i dates
        restWeeks = group 7 (drop i dates)
        cal = stack (row (take 7 name) : blankRow count `beside` row firstWeek : map row restWeeks)
        

months:: [(String,Int)]
months = [("January",31),("February",28),("March",31),("April",30)
         ,("May",31),("June",30),("July",31),("August",31)
         ,("September",30),("October",31),("November",30),("December",31)]
         
year = stack (intersperse (row [" "]) (map (lineUp . intersperse (col [" "])) (group 3 (map (month "Tue") months))))

