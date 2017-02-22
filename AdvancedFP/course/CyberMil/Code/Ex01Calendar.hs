module Ex01Calendar where

import Excell

-- Name of pair programmer 1 ___________________________

-- Name of pair programer 2  ___________________________

{-
1) Got to the programming resource links. Find the libraries.
   Right click on "Excell.hs" and save it in the same 
   directory where you saved this file.
   
2) While you're there also right-click and download
   the "CSV.hs" library and save it in the same 
   directory where you saved this file.
   
3) Write a program that creates an Excel table that looks 
   like a calendar for April 2009.
   

+---+---+---+---+---+---+---+
|A  |P  |R  |I  |L  |   |   |
+---+---+---+---+---+---+---+
|Sun|Mon|Tue|Wed|Thu|Fri|Sat|
+---+---+---+---+---+---+---+
|   |   |   |1  |2  |3  |4  |
+---+---+---+---+---+---+---+
|5  |6  |7  |8  |9  |10 |11 |
+---+---+---+---+---+---+---+
|12 |13 |14 |15 |16 |17 |18 |
+---+---+---+---+---+---+---+
|19 |20 |21 |22 |23 |24 |25 |
+---+---+---+---+---+---+---+
|26 |27 |28 |29 |30 |   |   |
+---+---+---+---+---+---+---+   

-}   



-- Each row of the table corresponds to a list.

month:: [ Char]
days:: [String]
week1,week2,week3,week4,week5:: [Integer]


-- Create naming declarations for each row with
-- the type designated above

month = undefined

days = undefined

week1 = undefined
week2 = undefined
week3 = undefined
week4 = undefined
week5 = undefined


-- Use the Excel functions
-- row, col, rows, cols, blankRow, blankCol, beside, above
-- to create a calendar table.

calendar = undefined

-- Extra credit, create calendars for other months
