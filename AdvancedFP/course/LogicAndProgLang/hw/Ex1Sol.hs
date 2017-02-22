module Ex1Sol where
import SimpleProp

addvars:: Ord a => Prop a -> [a] -> [a] 
addvars TruthP ans = ans
addvars AbsurdP ans = ans
addvars (LetterP x) ans = insert x ans
addvars (NotP x) ans = addvars x ans
addvars (AndP x y) ans = addvars x (addvars y ans)
addvars (OrP x y)  ans = addvars x (addvars y ans)
addvars (ImpliesP x y)  ans = addvars x (addvars y ans)

insert x [] = [x]
insert x (y:ys) | x==y = y:ys
                | x<y  = x:y:ys
                | x>y  = y:(insert x ys)

vars:: Ord a => Prop a -> [a] 
vars x = addvars x []

assigns:: [Int] -> [[(Int,Bool)]]
assigns [] = []
assigns [n] = [[(n,True)],[(n,False)]]
assigns (x:xs) = map ((x,True):) ys ++ map ((x,False):) ys
  where ys = assigns xs
  
taut:: Prop Int -> Bool
taut prop = and [ value (lift x) prop | x <- assigns (vars prop)]

lift pairs x = case lookup x pairs of
                 Just b -> b
                 other -> False
-- hint, you might want to use the function "value" from SimpleProp
-- and the library function "lookup"

equal :: Prop Int -> Prop Int -> Bool
equal p1 p2 =
  and [ value (lift x) p1 == value (lift x) p2
      | x <- assigns (vars (AndP p1 p2))
      ]
      
      