module Ex6Sol where

import Subst
import Tableau
import Control.Monad.State
import Formula
import Term


type Sub = String -> Term String String
closed:: Sub -> Tree -> [Formula String String String]
            -> (Sub, Tree, Bool)
closed s Leaf truths = (s,Leaf,False)
closed s (Direct p tree) truths =
  let p2 = subst s p
  in case someConj p2 truths of
       Just(s2,x,y) -> (s2 |=> s,Direct (subst (s2 |=> s) p) (Closed x y),True)
       Nothing -> let (s2,tree2,ok) = closed s tree (extendTruths (True,p2) truths)
                  in (s2 |=> s,Direct (subst (s2 |=> s) p) tree2,ok)
closed s (Branch x y) truths = (s2,Branch t1 t2, b1 && b2)
   where (s1,t1,b1) = closed s x truths
         (s2,t2,b2) = closed s1 y (map (subst s1) truths)
    

test x = tblock tree2
  where (tree,next) = runState (tabTree [notP x] (single (notP x))) 1
        (s,tree2,ok) = closed emptySubst tree []
