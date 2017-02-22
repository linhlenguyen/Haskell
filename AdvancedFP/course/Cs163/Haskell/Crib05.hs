module Crib05 where

import Excell


tab1 = col [1,2,3,4,5,6:: Integer]


tab2 = col "ABC"


tab3 = row [2.3,3.1416,0.0:: Double]


tab4 = row [True,4==7]

xs:: [[Integer]]
xs = [[1,2,3], [], [99,98,97,96],[50]]


tab5 = cols xs


tab6 = rows xs


tab7 = tab3 `above` tab4


tab8 = tab1 `beside` tab2


tab9 = blankRow 3 `beside` row "ABC"


tab10 = blankCol 2 `above` row "ABC"


tab11 = stack [tab3,tab4,tab6]


tab12 = lineUp [tab1, tab2,tab4]

