{-# LANGUAGE  DeriveDataTypeable   #-}

module Hw2Sol where

import FiniteSet
import Prop
import Minisat
import Data.Typeable

d1 = dimS ["Odd","Even"]

d2 = dim 10
-- Is 0 positive, if not then
d2b = Dim 9 Int (map LInt [1,2,3,4,5,6,7,8,9])

d3 = Dim (length [5..10]) Int (map LInt [5 .. 10])

data Days = Sun | Mon | Tue | Wed | Thu | Fri | Sat 
  deriving (Enum,Show,Read,Typeable,Eq)
d4 = Dim 7 (Enum "Days") (map (LCon "Days" . show) [Sun .. Sat])  

--------------------------------------------------------
describes (LString "Even",LInt n) = even n
describes (LString "Odd",LInt n) = odd n
describes (_,_) = False

s1 :: FiniteSet (Prop Int)
s1 = partial [d1,d3] f 
  where f [x,y] = if describes(indexToLit d1 x,indexToLit d3 y)
                     then Just TruthP
                     else Nothing
        f _ = Nothing
                     
s2 = universe [d2,d2] f
  where f [i,j] = g (indexToLit d2 i) (indexToLit d2 j)
        g (LInt n) (LInt m) = (n+m)

s3 = partial [d2,d2] f
  where f [i,j] = g (indexToLit d2 i) (indexToLit d2 j)
        g (LInt n) (LInt m) = if m==n+1 then Just True else Nothing
        g _ _ = Nothing
        d2 = dim 5

{-  This would be a very usefull function!
partialByLit:: [Dimension] -> ([Literal] -> Maybe a) -> FiniteSet a
partialByLit dims pred = 
   FA dims
      (fromList [ (i,j) 
                | i <- flatInts dims 
                , Just j <- [pred (zipWith indexToLit dims (kIndex dims i))] ])                  
-} 

s3b = partialByLit [d2,d2] g 
  where g [LInt n,LInt m] = if m==n+1 then Just True else Nothing
        g _ = Nothing
        d2 = dim 5
        
s4 = partial [d4,d4] f
  where f [i,j] = g (indexToLit d4 i) (indexToLit d4 j)
        g (LCon "Days" x) (LCon "Days" y) =
           if (successor((read x)::Days)) == (read y) then Just True else Nothing
        successor Sat = Sun
        successor x = succ x

-----------------------------------------------        
notLike :: Boolean t => t -> t
notLike x | isTrue x = false
notLike x | isFalse x = true
notLike x = neg x

andLike x y | isTrue x = y
andLike x y | isTrue y = x
andLike x y | isFalse x = false
andLike x y | isFalse y = false
andLike x y = conj x y

-----------------------------------------------
(_,trips) = enum f [d,d,d] 1
  where f [a,b,c] i = Just(i+1,LetterP i)
        d = dim 5

-----------------------------------------------        
c1 = complement trips
c2 = complement s3
c3 = intersect s3 (complement s3)
c4 = union s3 (complement s3)
test = full c4

--------------------------------------------------
people = ["Anita","Barbara","Caleb","Frank","George","Margareet","Tim","Walter"]

tuples = [ ("Frank","Tim"),("Tim" , "Caleb"),("Walter","Frank"), 
           ("Anita","Tim"),("Margareet","Barbara"),("Barbara","Caleb")]
                   
pd = dimS people           
p = fromFiniteList True [pd,pd] tuples
px = project [1,0] p
j1 = project [2,0,1] (join 1 p px)
j2 = project [1,0,2,3] (join 1 px j1)
ggp = project [0,3] j2

-- part 4 doesn't make sense, because you have to use the original 
-- (actually its flip) to make progress. Instead we need something
-- like this

step 0 p flip = p
step n p flip = step (n-1) (project [2,1] (join 1 p flip)) flip

go n p = step n p (project [1,0] p)

-- Transitive closure is like step, except it keeps all the
-- old values as well.

stepper p = if p==q then p else stepper q
  where q = union p (project [2,1] (join 1 p (project [1,0] p)))
  
-- you only need apply it until there is no change. But (log n)
-- where n is the size of P is always sufficient, though it may
-- reach a fixpoint way before that.

--------------------------------------------
-- Extra credit


addition = partialByLit [d,d,d] g 
   where g [LInt n,LInt m,LInt x] = if m+n==x then Just True else Nothing
         g _ = Nothing
         d = dim 11
         
maxR = partialByLit [d,d,d] g 
   where g [LInt n,LInt m,LInt x] = if x == max n m then Just True else Nothing
         g _ = Nothing
         d = dim 11         