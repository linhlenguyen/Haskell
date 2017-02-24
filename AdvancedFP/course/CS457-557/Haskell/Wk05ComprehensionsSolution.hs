module CreatingListsWithComprehensions  where

import Char(ord)


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


-- [('0',48),('1',49),('2',50),('3',51),('4',52),('5',53)]
-- hint  (ord '0') == 48
x6 = [ (c,ord c) | c <- "012345" ]

-- [[1],[1,2],[1,2,3],[1,2,3,4],[1,2,3,4,5]]
-- nested sequences and comprhensions
x7 = [ [1..n] | n <- [1..5]]


days = ["Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"]

-- ["Monday","Tuesday","Wednesday","Thursday","Friday"]
-- hint use a comprehension with a filter
weekday = [ day | day <- days, not(day == "Sunday" || day=="Saturday")]


-- [[1,2,3,4,5,6,7]
-- ,[8,9,10,11,12,13,14]
-- ,[15,16,17,18,19,20,21]
-- ,[22,23,24,25,26,27,28]]
-- hint: nest a sequence in a comprehension
--       think about  (i*7 + 1)  and  (i*7 + 7)

x9 = [ [ i*7 + 1  .. i*7 + 7 ] | i <- [0,1,2,3] ]


-- ["MonDay","TuesDay","WeDnesDay","ThursDay","FriDay"]
-- Note all lowercase 'd' turned to upper case 'D'
-- hint: nested comprehensions
--        if c=='d' then 'D' else c

x10 = [ [ if c=='d' then 'D' else c | c <- day] | day <- weekday ]
