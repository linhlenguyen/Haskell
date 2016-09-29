class Functor f where
fmap :: (a -> b) -> f a -> f b

instance Functor  Maybe where 
fmap Nothing _ = Nothing
fmap _ Nothing = Nothing
fmap f Just a = Just f a

instance Functor Tree where  
fmap f EmptyTree = EmptyTree  
fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub) 

fmap (*2) EmptyTree  
fmap (*4) (foldr treeInsert EmptyTree [5,7,3,2,1,7])  
	
data Either a b = Left a | Right b
instance Functor (Either a) where
fmap f (Left x) = Left x
fmap f (Right x) = Right (f x)

instance Functor [] where 
map f [] = []
map f [x] = [f x]

instance Functor (+ a) where
fmap f (+a) = + (f a)

map :: (a -> b) -> [a] -> [b]

class (Functor f) => Applicative f where
pure :: a -> f a
<*> :: f(a -> b) -> f a -> f b

instance Applicative Maybe where
pure = Just
Nothing <*> _ = Nothing 
_ <*> Nothing = Nothing
Just f <*> a :: fmap f a

instance Applicative [] where 
pure = [x]
fs <*> xs = [f a | f <- fs, a <- xs]

--1 to 1 map
instance Applicative [] where
pure a = [a]
[] <*> _ = []
_ <*> [] = []
(<*>)(f:fs)(x:xs) = f x : ((<*>) fs xs)

instance (Num a) => Applicative (+ a) where 
pure = a + x
(+ a) f <*> (+ a) b = (+ a) (f b)

(+) <$> (+3) <*> (*100)
(+) <$> (+3) <*> (*100) $ 5
(+) ((+3)5) ((*100)5)

instance Applicative ((->) r) where
	pure x = (\_ -> x)
    f <*> g = \x -> f x (g x)
	
(+3) <*> (*100) 
\x -> (+3) x ((*100) x)

instance Functor ((->) r) where
	fmap = (.)

-- Functor is something that can me mapped over! 
-- Why is it (a -> b) -> f a -> f b
-- a -> b
-- f . g 
-- g :: (Num a) => a -> a
-- f :: (Num a) => a -> a
-- if function ((->) r) is a functor (lets call this g)
-- That means we can apply some f to some instance of g that will result in another function
(+3).(+5) 5 -- satisfy definition of functor!
zipWith :: (a->b->c) -> [a] -> [b] -> [c]
zipWith f [x] [y:ys] = f x y
zipWith f (x:xs) [y] = f x y
zipWith f (x:xs) (y:ys) = f x y : zipWith( f xs ys )

newtype ZipList a = ZipList { getZipList :: [a] } --putting a list inside a box 

instance Functor ZipList where
fmap f (ZipList x) = ZipList (fmap f x)

instance Applicative ZipList where
pure x = ZipList (repeat x)
ZipList fs <*> ZipList xs = ZipWith f fs xs
ZipList xs1 <*> ... <*> ZipList xsn = ZipWithn (ZipList ( f xs 1 .. xsn ))

ZipWith (+) [1,2,3] [4,5] 
[5,6]

data Maybe a = Nothing | Just a




	
