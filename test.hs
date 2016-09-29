dict = [("A", [1,2,3,4]),("B", [1,2,3]),("C", [1,2]),("D", [1,2,3,4])]

newA :: (Eq k) => k -> [(k,[a])] -> [(k,[a])]
newA k x = map (\(k', ls) -> if k' == k then (k',tail ls) else (k',ls)) x

data Tree a = Empty | Node a (Tree a) (Tree a)

addElement :: (Eq a) => Tree a -> a -> Tree a
addElement Empty c = Node a Empty Empty
addElement (Node x lhs rhs) c
                            | x >= c -> Node x lhs (addElement rhs c)
                            | otherwise = Node x (addElement lhs c) rhs

removeElement :: (Eq a) => Tree a -> a -> Tree a
removeElement Empty _ = Empty
removeElement (Node x lhs rhs) c
                           | x == c -> 

toTree :: (Eq a) => [a] -> Tree a
toTree
