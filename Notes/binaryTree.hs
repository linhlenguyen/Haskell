{-# LANGUAGE GADTs #-}

data BTree a = Empty | Node a (BTree a) (BTree a) deriving (Show, Read, Eq)

data BTree' a where
						Empty' :: BTree' a
						Node' :: a -> BTree' a -> BTree' a -> BTree' a
						deriving Show

--instance (Ord a) => Ord (BTree a) where
--(Node x l1 r1) <= (Node y l2 r2) = x <= y

addElement :: (Ord a) => a -> BTree a -> BTree a -- It's a type constructor!
addElement x Empty = Node x Empty Empty
addElement x (Node y lhs rhs)
	| x == y 	= 	Node x lhs rhs
	| x < y 	= 	Node y (addElement x lhs) rhs
	| x > y 	= 	Node y lhs (addElement x rhs)

mergeTree :: (Ord a) => BTree a -> BTree a -> BTree a
mergeTree x Empty = x
mergeTree (Node x lx rx) (Node y ly ry)
	| x < y = Node y (mergeTree (Node x lx rx) ly) ry
	| x > y = Node y ly (mergeTree (Node x lx rx) ry)

--Change root (re-root)

--if x <= y then (Node y (addElement (Node x l1 r1) lhs) rhs) else (Node y lhs (addElement (Node x l1 r1) rhs))

--data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)
--data List a = Empty | Cons { head :: a, tail :: List a } deriving (Show, Read, Eq, Ord)

--data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

{-
data Tree a = Empty | Node a [Tree a] deriving (Show, Read, Eq)

data IPCRule = IPCRule {}

instance Ord IPCRule where
(compare) IPCRule {} IPCRule =
-}
