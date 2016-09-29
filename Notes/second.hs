class Functor f where
fmap :: (a -> b) -> f a -> f b

instance Functor [] where
fmap f x:xs = f x : fmap xs

instance Functor Maybe where
fmap f Just a = Just f a
fmap _ Nothing = Nothing

--Why is function type present this way
--Functor takes in a type, function takes in a type and returns another type 

instance Functor ((->) r) where
fmap = (.) 
(.) :: (a -> b) -> (b -> c) -> (a -> c)
f . g = \x -> f (g x)
-- (b -> c) ~ f b ~ ((->) c) b ?
-- (a -> c) ~ f a
fmap f g = f.g

class (Functor f) => Applicative f where
pure :: a -> f a
(<*>) :: f (a -> b) -> f a -> f b

instance Applicative Maybe where
pure = Just
_ <*> Nothing = Nothing
Just f <*> Just a = Just f a

instance Applicative [] where
pure x = [x]
f <*> x = [fs xs | fs <- f, xs <- x] --look at list comprehension and Monad

instance Applicative ((->)r) where
pure x = \_ -> x
f <*> g = \x -> f x (g x)
--applying x to f to unbox f? cause f is a->(b->c), when applying a to f you will get b->c
(a -> (b -> c)) -> (a -> b) -> (a -> c)

pure (+) <*> (+3)
pure (+) = \_ -> (+)
let oddFunc = \x -> (+) (x+3) :: a->b->c

(\_ -> a -> b -> c) -> (d -> e) 
(a -> b -> c) -- f x
((d -> e) -> b -> c) -- f x g x  
let oddFunc3 = oddFunc 3 
\x -> x + (3+3)

(+).(+3) <*> (*100) :: (Num a) => (a -> a -> a) -> (a -> a) -> (a -> a)
\x -> (+) (x+3) (x*100)
pure (\x -> (+) (x+3) (x*100)) <*> (-5) 
\_ -> (\x -> (+) (x+3) (x*100)) --applied with x -> unboxing!
\x -> (\x -> (+) (x+3) (x*100) (x-5)
 
(+) <$> (+3) 
(\_ -> (+)) <*> (+3)
\x -> (+) ((+3) x)

(\x -> (+) ((+3) x)) <*> (-5)
\y ->  (+) ((+3) y) ((-5) y)
