{-# Language GADTs, DataKinds, KindSignatures, 
    RankNTypes, TypeOperators, PolyKinds #-}

module GADTexamples  where

import Prelude hiding (Left,Right)

data Nat = Zero | Succ Nat

data Z
data S n

-------------------------------------------------
-- Witneses of Odd and Even

data Even n where
  Base:: Even Z
  NE:: Odd n -> Even (S n)
  
data Odd n where
  NO :: Even n -> Odd (S n)
  
odd1 = NO Base
even4 = NE (NO (NE (NO Base)))

data EvenX:: Nat -> * where
  BaseX:: EvenX Zero
  NEX:: OddX n -> EvenX (Succ n)
  
data OddX:: Nat -> * where
  NOX:: EvenX n -> OddX (Succ n)
  
----------------------------------------
-- length indexed lists

data LList :: * -> * -> * where
  LNil:: LList a Z
  LCons:: a -> LList a n -> LList a (S n)

data Vec:: * -> Nat ->  * where
  Nil :: Vec a Zero
  Cons:: a -> Vec a n -> Vec a (Succ n)
  
l0 = Nil
l1 = Cons 4 Nil
l3 = Cons 4 (Cons 5 (Cons 1 Nil))

hd:: Vec a (Succ n) -> a
hd (Cons x xs) = x

mapp:: (a -> b) -> Vec a n -> Vec b n
mapp f Nil = Nil
mapp f (Cons x xs) = Cons (f x) (mapp f xs)

----------------------------------------------
-- Balanced Trees

{- 
A red-black tree is a binary search tree with the following additional invariants:
        Each node is colored either red or black
        The root is black
        The leaves are black
        Each Red node has Black children
        for all internal nodes, each path from that node to a descendant 
                leaf contains the same number of black nodes.

We can encode these invariants by thinking of each internal
node as having two attributes: a color and a black-height.
-}

data Color = Red | Black
 
data SubTree :: Color -> Nat -> * where
   LeafRB :: SubTree Black Zero
   RNode :: (SubTree Black n) -> Int -> (SubTree Black n) -> SubTree Red n
   BNode :: (SubTree cL m) -> Int -> (SubTree cR m) -> SubTree Black (Succ m)
  
data RBTree where
  Root:: (forall n. (SubTree Black n)) -> RBTree

---------------------------------------------
-- AVL Trees
-- In an AVL tree, the heights of the two child subtrees of 
-- any node differ by at most one; 

data Balance:: Nat -> Nat -> Nat -> * where
  Same :: Balance n n n
  Less :: Balance n (Succ n) (Succ n)
  More :: Balance (Succ n) n (Succ n)

data Avl:: Nat -> * where
  TipA:: Avl Zero
  NodeA:: Balance i j k -> Avl i -> Int -> Avl j -> Avl (Succ k)

data AVL = forall h. AVL (Avl h)

data (+) a b = L a | R b
unreachable = error "Code that should never be executed was pulled on."  

insert :: Int -> AVL -> AVL
insert x (AVL t) = case ins x t of L t -> AVL t; R t -> AVL t

ins :: Int -> Avl n -> (Avl n + Avl (Succ n))
ins x TipA = R(NodeA Same TipA x TipA)
ins x (NodeA bal lc y rc)
  | x == y = L(NodeA bal lc y rc)
  | x < y  = case ins x lc of
               L lc -> L(NodeA bal lc y rc)
               R lc ->
                 case bal of
                   Same -> R(NodeA More lc y rc)
                   Less -> L(NodeA Same lc y rc)
                   More -> rotr lc y rc -- rebalance
  | x > y  = case ins x rc of
               L rc -> L(NodeA bal lc y rc)
               R rc -> 
                 case bal of
                   Same -> R(NodeA Less lc y rc)
                   More -> L(NodeA Same lc y rc)
                   Less -> rotl lc y rc -- rebalance

rotr :: Avl(Succ(Succ n)) -> Int -> Avl n -> (Avl(Succ(Succ n))+Avl(Succ(Succ(Succ n))))
-- rotr Tip u a = unreachable
rotr (NodeA Same b v c) u a = R(NodeA Less b v (NodeA More c u a))
rotr (NodeA More b v c) u a = L(NodeA Same b v (NodeA Same c u a))
-- rotr (NodeA Less b v TipA) u a = unreachable
rotr (NodeA Less b v (NodeA Same x m y)) u a = L(NodeA Same (NodeA Same b v x) m (NodeA Same y u a))
rotr (NodeA Less b v (NodeA Less x m y)) u a = L(NodeA Same (NodeA More b v x) m (NodeA Same y u a))
rotr (NodeA Less b v (NodeA More x m y)) u a = L(NodeA Same (NodeA Same b v x) m (NodeA Less y u a))

rotl :: Avl n -> Int -> Avl(Succ(Succ n)) -> (Avl(Succ(Succ n))+Avl(Succ(Succ(Succ n))))
-- rotl a u TipA = unreachable
rotl a u (NodeA Same b v c) = R(NodeA More (NodeA Less a u b) v c)
rotl a u (NodeA Less b v c) = L (NodeA Same (NodeA Same a u b) v c)
-- rotl a u (NodeA More TipA v c) = unreachable
rotl a u (NodeA More (NodeA Same x m y) v c) = L(NodeA Same (NodeA Same a u x) m (NodeA Same y v c))
rotl a u (NodeA More (NodeA Less x m y) v c) = L(NodeA Same (NodeA More a u x) m (NodeA Same y v c))
rotl a u (NodeA More (NodeA More x m y) v c) = L(NodeA Same (NodeA Same a u x) m (NodeA Less y v c))


delMin :: Avl (Succ n) -> (Int, (Avl n + Avl (Succ n)))
-- delMin TipA = unreachable
delMin (NodeA Less TipA x r) = (x,L r)
delMin (NodeA Same TipA x r) = (x,L r)
-- delMin (NodeA More TipA x r) = unreachable
delMin (NodeA bal (l@(NodeA _ _ _ _)) x r) = 
      case delMin l of
        (y,R l) -> (y,R(NodeA bal l x r))
        (y,L l) ->
          case bal of
            Same -> (y,R(NodeA Less l x r))
            More -> (y,L(NodeA Same l x r))
            Less -> (y,rotl l x r)
            
------------------------------- -- 2-3 trees 

--  a tree where every node with children (internal node)
-- has either two children (2-node) and one data element or
-- three children (3-nodes) and two data elements. Nodes on
-- the outside of the tree (leaf nodes) have no children and
-- zero, one or two data elements

data Tree23:: Nat -> * -> * where
  Three :: (Tree23 n a) -> a -> (Tree23 n a) -> a -> (Tree23 (Succ n) a)
  Two:: (Tree23 n a) -> a -> (Tree23 n a) -> (Tree23 (Succ n) a)
  Leaf1 :: a -> (Tree23 Zero a)
  Leaf2 :: a -> a -> (Tree23 Zero a)
  Leaf0 :: (Tree23 Zero a)
  
data Ans n a where 
  Static :: Tree23 n a -> Ans n a
  Grow :: Tree23 (Succ n) a -> Ans n a
  
insert23 :: Ord a => a -> Tree23 n a -> Ans n a  
insert23 a Leaf0 = Static(Leaf1 a)
insert23 a (Leaf1 b) 
  | a <= b = Static (Leaf2 a b)
  | a > b  = Static (Leaf2 b a)
insert23 a (Leaf2 b c) 
  | a <= b = Grow(Two (Leaf1 a) b (Leaf1 c))
  | a >= c = Grow(Two (Leaf1 b) c (Leaf1 a))
  | a > b && a < c = Grow(Two (Leaf1 b) a (Leaf1 c))
insert23 a (Two t1 x t2)
  | a < x = case insert23 a t1 of
              Static t3 -> Static(Two t3 x t2)
              Grow t3 -> undefined
              
-----------------------------------------
-- An equality type witness

data Equal:: k -> k -> * where
  Refl :: Equal x x

-----------------------------------------
-- Representation types

data Rep:: * -> * where
 Int:: Rep Int
 Char:: Rep Char
 Float:: Rep Float
 Bool :: Rep Bool
 Pair:: Rep a -> Rep b -> Rep (a,b)
 List :: Rep a -> Rep [a]

-- A simple sort of generic programming

eq:: Rep a -> a -> a -> Bool 
eq Int  x y  = x==y
eq Char x y  = x==y
eq Float x y = x==y
eq Bool  x y = x==y
eq (Pair t1 t2)(a,b) (c,d) = (eq t1 a c) &&  (eq t2 b d)
eq (List t) xs ys 
  | not(length xs == length ys) = False
  | otherwise = and (zipWith (eq t) xs ys)
  
-- Computing if two Reps are equal

test:: Rep a -> Rep b -> Maybe(Equal a b)
test Int Int = Just Refl
test Char Char = Just Refl
test Float Float = Just Refl
test Bool Bool = Just Refl
test (Pair x y) (Pair m n) =
  do { Refl <- test x m
     ; Refl <- test y n
     ; Just Refl }
test (List x) (List y) = 
  do { Refl <- test x y
     ; Just Refl}
test _ _ = Nothing     

----------------------------------------
-- Well typed terms

data Exp:: * -> * where
  IntE :: Int -> Exp Int
  CharE:: Char -> Exp Char
  PairE :: Exp a -> Exp b -> Exp (a,b)
  VarE:: String -> Rep t -> Exp t
  LamE:: String -> Rep t -> Exp s -> Exp (t -> s)
  ApplyE:: Exp (a -> b) -> Exp a -> Exp b
  FstE:: Exp(a,b) -> Exp a
  SndE:: Exp(a,b) -> Exp b

data Env where
  Empty :: Env
  Extend :: String -> Rep t -> t -> Env -> Env

-- A tagless interpreter  

eval:: Exp t -> Env -> t  
eval (IntE n) env = n
eval (CharE c) env = c
eval (PairE x y) env = (eval x env, eval y env)
eval (VarE s t) Empty = error ("Variable not found: "++s)
eval (LamE nm t body) env = (\ v -> eval body (Extend nm t v env)) 
eval (ApplyE f x) env = (eval f env) (eval x env)
eval (FstE x) env = fst(eval x env)
eval (SndE x) env = snd(eval x env)
eval (v@(VarE s t1)) (Extend nm t2 value more) 
  | s==nm = case test t1 t2 of 
              Just Refl -> value
              Nothing -> error "types don't match"
  | otherwise = eval v more

  
---------------------------------------------
-- well formed paths in trees

data Shape = Tp | Nd | Fk Shape Shape

data Path :: Shape -> * where
  Here:: Path Nd 
  Left:: Path x  -> Path (Fk x y) 
  Right:: Path y -> Path (Fk x y) 
 

data Tree:: Shape -> * -> * where
  Tip :: Tree Tp a
  Node:: a -> Tree Nd a
  Fork :: Tree x a -> Tree y a -> Tree (Fk x y) a

find:: Eq a => a -> Tree sh a -> [Path sh]  
find n Tip = []
find n (Node m) =
  if n==m then [Here] else []
find n (Fork x y) = 
 (map Left (find n x)) ++ (map Right (find n y))

extract:: Eq a => Tree sh a -> Path sh -> a
extract (Node n) (Here) = n
extract (Fork l r) (Left p) = extract l p
extract (Fork l r) (Right p) = extract r p
-- No other cases are possible, 
-- Since there are no Paths with index Tp
  
-------------------------
data TempUnit = Fahrenheit | Celsius | Kelvin

data Degree:: TempUnit -> * where
 F:: Float -> Degree Fahrenheit
 C:: Float -> Degree Celsius
 K:: Float -> Degree Kelvin

add:: Degree u -> Degree u -> Degree u
add (F x) (F y) = F(x+y)
add (C x) (C y) = C(x+y)
add (K x) (K y) = K(x+y)

-----------------------------------------------
-- Nway Map

data Zip :: * -> * -> * where
  Z:: Zip a [a]
  S:: Zip b c ->  Zip (a -> b) ([a] -> c)
  
help:: Zip a b -> a -> b -> b
help Z x xs = x:xs
help (S n) f rcall =
 (\ ys -> case ys of
           (z:zs) -> help n (f z) (rcall zs)
           other -> skip n)           

skip:: Zip a b -> b
skip Z = []
skip (S n) = \ ys -> skip n

zipN:: Zip a b -> a -> b
zipN Z = \ n -> [n]
zipN (n@(S m)) = let zip f = help n f (\ x -> zip f x)
                 in zip


test5 = zipN (S(S(S Z))) (\ x y z -> (x+z,y))[2,3] [5,1] [6,8]


