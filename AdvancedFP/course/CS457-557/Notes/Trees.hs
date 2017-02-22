data Tree a = Leaf a | Branch (Tree a) (Tree a)
  deriving Show

data IntegerTree = IntLeaf Integer
                 | IntBranch IntegerTree IntegerTree

data SimpleTree = SLeaf
                | SBranch SimpleTree SimpleTree

data InternalTree a = ILeaf
                    | IBranch a (InternalTree a) (InternalTree a)

data FancyTree a b = FLeaf a
                   | FBranch b (FancyTree a b) (FancyTree a b)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f (Leaf x) = Leaf (f x)
mapTree f (Branch t1 t2) = Branch (mapTree f t1) (mapTree f t2)

t1 = Branch (Branch (Leaf "the") (Leaf "dog")) (Leaf "barked")
t2 = mapTree length t1

fringe :: Tree a -> [a]
fringe (Leaf x) = [x]
fringe (Branch t1 t2) = fringe t1 ++ fringe t2

treeSize :: Tree a -> Integer
treeSize (Leaf x) = 1
treeSize (Branch t1 t2) = treeSize t1 + treeSize t2

treeHeight :: Tree a -> Integer
treeHeight (Leaf x) = 0
treeHeight (Branch t1 t2) = 1 + max (treeHeight t1) (treeHeight t2)

foldTree :: (a -> a -> a) -> (b -> a) -> Tree b -> a
foldTree b l (Leaf x) = l x
foldTree b l (Branch t1 t2) = b (foldTree b l t1) (foldTree b l t2)

mapTree2 f = foldTree Branch (Leaf . f)
fringe2 = foldTree (++) (\ x -> [x])
treeSize2 = foldTree (+) (const 1)
treeHeight2 = foldTree (\ x y -> 1 + max x y) (const 0)

{-
leftBad :: Integer -> a -> a -> Tree a
leftBad 0 a b = Leaf a
leftBad (n+1) a b = Branch (leftBad n a b) (Leaf b)
-}

-- Previously called fringe
flatten :: Tree a -> [a]  
flatten t = flat t []

flat (Leaf x) xs = x:xs
flat (Branch a b) xs = flat a (flat b xs)

data Expr = C Float
  | Expr :+ Expr
  | Expr :- Expr
  | Expr :* Expr
  | Expr :/ Expr
 deriving Show

e1 = (C 10 :+ (C 8 :/ C 2)) :* (C 7 :- C 4)

evaluate :: Expr -> Float
evaluate (C x) = x
evaluate (e1 :+ e2) = evaluate e1 + evaluate e2
evaluate (e1 :- e2) = evaluate e1 - evaluate e2
evaluate (e1 :* e2) = evaluate e1 * evaluate e2
evaluate (e1 :/ e2) = evaluate e1 / evaluate e2

sumFromN n = C n :+ (sumFromN (n+1))
sumAll = sumFromN 1

add1 (C n) = C (n+1)
add1 (x :+ y) = add1 x :+ add1 y
add1 (x :- y) = add1 x :- add1 y
add1 (x :* y) = add1 x :* add1 y
add1 (x :/ y) = add1 x :/ add1 y

sumAll2 = C 1 :+ (add1 sumAll2)

showE 0 _ = "..."
showE n (C m) = show m
showE n (x :+ y) = "(" ++ (showE (n-1) x) ++ "+" ++ (showE (n-1) y) ++ ")"

