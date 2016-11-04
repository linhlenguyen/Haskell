module UsingParens where

-- In this worksheet replace each  "X2 = undefined" with "X2 = fully-parenthesiszed-expression"
-- such that X1 and X2 evaluate to the same value. 
-- 
-- You may want to consult the table of precedences.
-- ------+-----------------------+-----------------------+--------------------+
-- Prec- |  Left associative     | Non-associative       | Right associative  |
-- edence|  operators            | operators             | operators          |
-- ------|-----------------------|-----------------------|--------------------+
-- 9     | !!              .     |                       |                    |
-- ------|-----------------------|-----------------------|--------------------+
-- 8     |                       |                       | ^    ^^    **      |
-- ------|-----------------------|-----------------------|--------------------+
-- 7     | *   /  `div`          |                       |                    |
--       | `mod`  `rem`  `quot`  |                       |                    |
-- ------|-----------------------|-----------------------|--------------------+    
-- 6     | +   -                 |                       |                    |
-- ------|-----------------------|-----------------------|--------------------+
-- 5     |                       |                       | :  ++              |
-- ------|-----------------------|-------------------- --|--------------------+
-- 4     |                       | ==  /=   <   <=   >   |                    |
--       |                       | >=  `elem`  `notElem` |                    |
-- ------|-----------------------|-----------------------|--------------------+
-- 3     |                       |                       | &&                 |
-- ------|-----------------------|-----------------------|--------------------+
-- 2     |                       |                       | ||                 |
-- ------|-----------------------|-----------------------|--------------------+
-- 1     | >>   >>=              |                       |                    |
-- ------|-----------------------|-----------------------|--------------------+
-- 0     |                       |                       | $    $!   `seq`    |
-- ------|-----------------------|-----------------------|--------------------+



-- For example I have done "a2" below

a1 = 3 + 5 * 5
a2 = (3 + (5 * 5))


------------------------------
b1 = 5 < 2 + 4
b2 = (5 < (2 + 4))

------------------------------
c1 = length [2] + 5 * 2
c2 = ((length [2]) + (5 * 2))

-----------------------------
d1 = 5 ^ id 3
d2 = (5 ^ (id 3))

-----------------------------
e1 = [2,3,4] !! 2 ^ 5
e2 = (([2,3,4] !! 2) ^ 5)

-----------------------------
f1 = 3 > 4  &&  2 == 5 * 3
f2 = ((3 > 4)  &&  (2 == (5 * 3)))

------------------------------
g1 = True && 3 `elem` [3,4,5]
g2 = (True && (3 `elem` [3,4,5]))


---------------------------------------------------------------------
-- if when you type "main" you get True, then you have succeeded

main = all true [a1==a2, b1==b2,c1==c2,d1==d2,e1==e2,f1==f2,g1==g2]
  where true x = x==True

