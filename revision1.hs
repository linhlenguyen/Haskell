map :: (a -> b) -> [a] -> [b]
map f (x:xs) = f x : map f xs

class Applicative r where
pure :: a -> r a
<*> :: r (a -> b) -> r a -> r b

instance Applicative [] where
pure x = [x]
(f:fs) <*> x = concat (map f x : fs <*> x)
