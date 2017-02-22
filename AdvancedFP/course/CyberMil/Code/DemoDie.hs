module DemoDie where

import Excell
import List(sort,group)


-- tab1 = addCT (map (* 780) [1,1,1,1,1,1::Int]) emptyTable

rolls1 = [ x | x <- [1..6]]
groups1 = group (sort rolls1)
counts1 = [ (length xs)* 780 | xs <- groups1]
tab1 = col counts1

rolls2 = [ x+y | x <- [1..6], y <- [1..6]]
groups2 = group (sort rolls2)
counts2 = [ (length xs)* 130 | xs <- groups2]
tab2 = blankCol 1 `above` col counts2

rolls3 = [ x+y+z | x <- [1..6], y <- [1..6], z <- [1..6]]
group3 = group (sort rolls3)
counts3= [ (length xs)*  29 | xs <- group3]
tab3 = blankCol 2 `above` col counts3 

rolls4 = [ w+x+y+z | w <- [1..6], x <- [1..6], y <- [1..6], z <- [1..6]]
group4 = group (sort rolls4)
counts4= [ (length xs)* 5  | xs <- group4]
tab4 = blankCol 3 `above` col counts4

rolls5 = [ v+w+x+y+z | v <- [1..6], w <- [1..6], x <- [1..6], y <- [1..6], z <- [1..6]]
group5 = group (sort rolls5)
counts5= [ (length xs) * 1 | xs <- group5]
tab5 = blankCol 4 `above` col counts5

tab6 = col [1..30::Int] `beside` tab5 `beside` tab4 `beside` tab3 `beside` tab2 `beside` tab1

main = export "rollNDie" tab6
