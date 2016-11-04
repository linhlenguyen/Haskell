module CreatingListsWithComprehensions  where

import Char(ord)

-- For each named declaration below. Use the comment preceeding
-- it as a guide to creating a comprehension that computes the
-- same value as that displayed in the comment.

-- [8,9,10,11,12]
x1 = undefined


-- [20,19,18,17,16]
x2 = undefined


-- [10,20,30,40,50,60]
x3 = undefined

-- [500,450,400,350]
x4 = undefined


-- [(1,1),(1,2),(1,3),(2,1),(2,2),(2,3)]
x5 = undefined


-- [(1,99),(2,99),(3,99),(4,99)]
x6 = undefined


-- -- [(1,2),(2,3),(3,4),(4,5)]
x7 = undefined


-- [('0',48),('1',49),('2',50),('3',51),('4',52),('5',53)]
-- hint  (ord '0') == 48
x8 = undefined

-- [[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5]]
-- nested sequences and comprehensions
x9 = undefined


days = ["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]


-- ["Monday","Tuesday","Wednesday","Thursday","Friday"]
-- hint use a comprehension with a filter
weekday = undefined


-- [[1,2,3,4,5,6,7]
-- ,[8,9,10,11,12,13,14]
-- ,[15,16,17,18,19,20,21]
-- ,[22,23,24,25,26,27,28]]
-- hint: nest a sequence in a comprehension
--       think about  (i*7 + 1)  and  (i*7 + 7)
x10 = undefined


-- ["MonDay","TuesDay","WeDnesDay","ThursDay","FriDay"]
-- Note all lowercase 'd' turned to upper case 'D'
-- hint: nested comprehensions
--        if c=='d' then 'D' else c
x11 = undefined

-- write a function, that given an integer, n, returns the sequence
-- of pairs of array indices necessary to perform the bubble
-- sort on an array with indices from 1 to n.
-- bubbleIndices 4 = [(1,2),(2,3),(3,4),(1,2),(2,3),(1,2)]

bubbleIndices:: Int -> [(Int,Int)]
bubbleIndices n = undefined

-- write a function, that given an integer, n, returns the sequence
-- of pairs of array indices necessary to perform the exchange
-- sort on an array with indices from 1 to n.
-- exchangeIndices 5 = [(1,2),(1,3),(1,4),(1,5),(2,3),(3,4),(4,5),(3,4),(3,5),(4,5)]

exchangeIndices:: Int -> [(Int,Int)]
exchangeIndices n = undefined
