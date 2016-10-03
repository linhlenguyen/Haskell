dict = [("A", [1,2,3,4]),("B", [1,2,3]),("C", [1,2]),("D", [1,2,3,4])]

newA :: (Eq k) => k -> [(k,[a])] -> [(k,[a])]
newA k x = map (\(k', ls) -> if k' == k then (k',tail ls) else (k',ls)) x

data Tree a = Empty | Node a (Tree a) (Tree a)

addElement :: (Ord a) => Tree a -> a -> Tree a
addElement Empty c = Node c Empty Empty
addElement (Node x lhs rhs) c
                            | x >= c = Node x lhs (addElement rhs c)
                            | otherwise = Node x (addElement lhs c) rhs

removeElement :: (Ord a) => Tree a -> a -> Tree a
removeElement Empty _ = Empty
removeElement (Node x lhs rhs) c = Node x Empty Empty

type Radius = Float
type Point = (Float, Float)
data Circle = Circle Point Radius

class Shape s where
  perimeter :: s -> Float

instance Shape Circle where
  perimeter (Circle (x,y) r) =  2*pi*r



--toTree :: (Eq a) => [a] -> Tree a
--toTree
