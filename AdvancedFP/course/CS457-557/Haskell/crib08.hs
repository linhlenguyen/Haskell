module Crib08 where
-- Simple data declaration, 
-- case expression, and patterns

data Tree a
  = Tip a
  | Node2 (Tree a) a (Tree a)
  | Node3 (Tree a) (Tree a) (Tree a)

-- definition by case expression  
treeToList x = 
  case x of
    Tip a -> [a]
    Node2 t1 a t2 -> 
      treeToList t1 ++ [a] ++ 
      treeToList t2
    Node3 t1 t2 t3 -> 
       treeToList t1 ++ 
       treeToList t2 ++
       treeToList t3

-- definition by pattern matching       
sumTree (Tip a) = a
sumTree (Node2 t1 x t2) = 
   sumTree t1 + x + sumTree t2
sumTree (Node3 x y z) = 
   sumTree x + sumTree y + sumTree z