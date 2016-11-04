

data Shape :: *1 where
  Tp:: Shape
  Nd:: Shape
  Fk:: Shape ~> Shape ~> Shape

data Tree :: Shape ~> *0 ~> *0 where
  Tip:: Tree Tp a
  Node:: a -> Tree Nd a
  Fork:: Tree x a -> Tree y a -> Tree (Fk x y) a

data Path:: Shape ~> *0 ~> *0 where
  None :: Path Tp a
  Here :: b -> Path Nd b
  Left :: Path x a -> Path (Fk x y) a
  Right:: Path y a -> Path (Fk x y) a

map f [] =[]
map f (x:xs) = f x : map f xs

find:: (a -> a -> Bool) -> a -> Tree s a -> [Path s a]
find eq n Tip = []
find eq n (Node m) =
  if eq n m then [Here n] else []
find eq n (Fork x y) =
  map Left (find eq n x) ++
  map Right (find eq n y)

extract:: Path sh a -> Tree sh a -> a