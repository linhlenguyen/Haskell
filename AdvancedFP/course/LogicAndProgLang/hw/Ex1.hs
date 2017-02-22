module HW1 where

import SimpleProp


addvars:: Ord a => Prop a -> [a] -> [a] 
addvars = undefined

vars:: Ord a => Prop a -> [a] 
vars = undefined

assigns:: [Int] -> [[(Int,Bool)]]
assigns = undefined

taut:: Prop Int -> Bool
taut = undefined
-- hint, you might want to use the function "value" from SimpleProp
-- and the library function "lookup"

equal :: Prop Int -> Prop Int -> Bool
equal = undefined