module ListEx  where

import List(inits)


subsets:: [a] -> [[a]]
subsets []= [[]]
subsets (x:xs)= subsets xs ++ map (x:) (subsets xs)