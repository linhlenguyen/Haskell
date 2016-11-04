module Tree23 where

------------------------------------------------------------
-- A 2-3 Tree has two kinds of branching (non-leaf) nodes.
-- They have either 2 or 3 sub trees.
-- There are also two kinds of leaf nodes that store either
-- one or two elements.

data Tree a
  = Empty
  | Leaf1 a  
  | Leaf2 a a 
  | Node1 (Tree a) a (Tree a)
  | Node2 (Tree a) a (Tree a) a (Tree a)

---------------------------------------------------------------
-- 2-3 Trees maintain a variation of the binary search tree invariant

-- 1) There are no duplicate lements in the tree.
-- 2) In a leaf node (Leaf x y) it is an invariant that x<y
-- 3) In a branching node (Node1 t1 x t2) it is an invariant
--    that all values found in t1 are less than x, and all
--    values found in t2 are greater than x
-- 4) In a branching node (Node2 t1 x t2 y t3) it is an invariant
--    that all values found in t1 are less than x, and all
--    values found in t2 are greater than x and less than y
--    and all values found in t3 are greater than y
-- 5) The Empty Tree is never stored as a sub tree of a Node1 
--    or Node2 (Or returned as the value of insert).

------------------------------------------------------------
-- finding values in a 2-3 Tree is just like binary search

find :: (Ord a) => a -> Tree a -> Bool
find n Empty = False
find n (Leaf1 x)
  | n==x = True
  | otherwise = False
find n (Leaf2 x y) 
  | n==x = True
  | n==y = True
  | otherwise = False
find n (Node1 t1 x t2)
  | n<x  = find n t1
  | n==x = True
  | n>x  = find n t2
find n (Node2 t1 x t2 y t3)
  | n<x  = find n t1
  | n==x = True
  | n<y  = find n t2  
  | n==y = True
  | n>y  = find n t3

-------------------------------------------------------------------

data Growth a b 
  = Same  a  
  | Taller b

-- When inserting a value into a 2-3 Tree, the tree may grow
-- in Height, or may increase in branching factor from 2 to 3.
-- We use the type Growth to record this information

insert::(Ord a) => a -> Tree a -> Growth (Tree a) (Tree a)
insert x Empty = Same (Leaf1 x)
insert x (t@(Leaf1 y)) 
  | x<y  = Same (Leaf2 x y)
  | x==y = Same t
  | x>y  = Same (Leaf2 y x)
insert x (t@(Leaf2 y z)) 
  | x<y  = Taller (Node1 (Leaf1 x) y (Leaf1 z))
  | x==y = Same t
  | x<z  = Taller (Node1 (Leaf1 y) x (Leaf1 z))
  | x==z = Same t
  | x>z  = Taller (Node1 (Leaf1 y) z (Leaf1 x))
insert x (t@(Node1 t1 y t2)) 
  | x<y  = case insert x t1 of
             Same t1' -> Same (Node1 t1' y t2)
             Taller (Node1 m b n) -> Same (Node2 m b n y t2)
             Taller (Node2 i b j c k) -> Taller (Node1 (Node1 i b j) c (Node1 k y t2)) 
  | x==y = Same t
  | x>y  = case insert x t2 of
             Same t2' -> Same (Node1 t1 y t2')
             Taller (Node1 m b n) -> Same (Node2 t1 y m b n)
             Taller (Node2 i b j c k) -> Taller (Node1 (Node1 t1 y i) b (Node1 j c k)) 
insert x (t@(Node2 t1 y t2 z t3))
  | x<y  = case insert x t1 of
             Same t1' -> Same (Node2 t1' y t2 z t3)
             Taller t1' -> Taller (Node1 t1' y (Node1 t2 z t3))
  | x==y = Same t
  | x<z  = case insert x t2 of
             Same t2' -> Same (Node2 t1 y t2' z t3)
             Taller (Node1 t4 w t5) -> Taller (Node1 (Node1 t1 y t4) w (Node1 t5 z t3))
             Taller (Node2 t4 u t5 v t6) -> Taller (Node1 (Node2 t1 y t4 u t5) v (Node1 t6 z t3))
  | x==z = Same t
  | x>z  = case insert x t3 of
             Same t3' -> Same (Node2 t1 y t2 z t3')
             Taller t3' -> Taller (Node1 (Node1 t1 y t2) z t3')

