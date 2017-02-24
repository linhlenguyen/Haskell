-- Fix the error in the file below. Load the file, read the error mesage
-- and fix ONE ERROR at a time. Then load the file and read the next error
-- message. My comments below hint at the fixes. DO NOT fix the errors 
-- without first re-loading the file for each mistake. Becomming familiar
-- with the error messages is the purpose of the worksheet.


module FixingErrors  where       -- every module declaration ends with what keyword?


a1:: Bool
a1 = True                       -- What type is True

a2 = 7 * 6                      -- where is seven defined?

a3 = 99                         -- what column should all naming declarations start in?
 
a4 = length [4.0]               -- can you take the length of 4.0?

a5 =  fromIntegral 3            -- Overloading requires you declare the type of a5

a6 = 0                          -- is something missing?

a7 = if a3==99 then 1 else 0    -- How do you test if two things are equal?

a8 = if 2<5 then "b" else "a"   -- Do the branches have the right type?

a9:: Int -> Int
a9 x = x `div` 2

a10 = a9 3

a11 = False && True             -- 4 is somekind of Num, but "&&" requires a Bool