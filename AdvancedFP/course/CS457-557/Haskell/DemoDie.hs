module DemoDie where

import Excell
import List(sort,group)

counts rolls scale = [ length xs * scale | xs <- group(sort rolls) ]
label n counts =  stack [col[show n++ dice n], blankCol (n-1)] `above` col counts

dice 1 = " die"
dice n = " dice"

rolls1 = [ x | x <- [1..6]]
tab1 scale = label 1 (counts rolls1 scale)

rolls2 = [ x+y | x <- [1..6], y <- [1..6]]
tab2 scale = label 2 (counts rolls2 scale)

rolls3 = [ x+y+z | x <- [1..6], y <- [1..6], z <- [1..6]]
tab3 scale = label 3 (counts rolls3 scale)

table3 = lineUp[blankCol 1 `above` col [1..18::Int],tab1 1,tab2 1,tab3 1]
test = export "roll3Die" table3


rolls4 = [ w+x+y+z | w <- [1..6], x <- [1..6], y <- [1..6], z <- [1..6]]
tab4 scale = label 4 (counts rolls4 scale)

rolls5 = [ v+w+x+y+z | v <- [1..6], w <- [1..6], x <- [1..6], y <- [1..6], z <- [1..6]]
tab5 scale = label 5 (counts rolls5 scale)


table5 = lineUp[blankCol 1 `above` col [1..30::Int],tab1 780,tab2 130,tab3 29,tab4 5,tab5 1]
main = export "roll5Die" table5
