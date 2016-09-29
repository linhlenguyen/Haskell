liftA2 :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c  
liftA2 f a b = f <$> a <*> b  

sequenceA :: (Applicative f) => [f a] -> f [a]  
sequenceA [] = pure []  
sequenceA (x:xs) = (:) <$> x <*> sequenceA xs  

sequenceA :: (Applicative f) => [f a] -> f [a]  
sequenceA = foldr (liftA2 (:)) (pure [])  
-- what does pure [] mean in this context?
-- from applicative definitions pure is just wrapping some type around an applicative functors

class Functor f where
fmap :: (a -> b) -> f a -> f b

class (Functor f) => Applicative f where
pure :: a -> f a
(<*>) :: f (a -> b) -> f a -> f b

instance Applicative [] where 
pure x = [x]
fs <*> xs [f x | f <- fs, x <- xs]

--Foldable
class Foldable f where
fold :: (a -> b -> b) -> b -> f a -> b 

class Monoid m where
mappend :: m a -> m a -> m a
mempty :: a -> m a

class Monad m where
return :: a -> m a
(>>=) :: m a -> (a -> m b) -> m b
(>>) :: m a -> m b -> m b
x >> y = x >>= \_ -> y
fail :: String -> m a

class (Functor f) => Applicative f where
pure :: a -> f a
(<*>) :: f (a -> b) -> f a -> f b

instance Functor ((->) r) where
fmap f1 f2 -> \x -> f1 (f2 x)

instance Applicative ((->) r) where
pure a = \_ -> a
f <*> g = \x -> f x (g x) 

(+) <$> (*3) 
(pure (+) <*> (*3)) x
\_ -> (+) x (x*3)
(+x) (x*3)

(+) <$> (*3) <*> (+5) -- return a function that take a number and 
\x -> 

--r->a->b	 b->a
--http://stackoverflow.com/questions/11810889/functions-as-applicative-functors-haskell-lyah

class Functor f where 
fmap :: (a -> b) -> f a -> f b

instance Functor ((->)r) where
fmap = (.)

instance Foldable [] where
fold f a x:xs = f a fold 

--type of 
--function composistion
(.) :: (b->c) -> (a->b) -> (a->c)
f . g = \x -> f (g x)

class Functor [] where 
fmap f x:xs = f x : fmap xs

instance Applicative Maybe where 
pure x = Just x
Just f <*> a = fmap f a

instance Applicative ((->)r) where
pure x = (\_ -> x)
f x (<*>) g y = f x (g y)  

pure (+) <*> (+3) <*> (*100)  5

type Pole = (Int, Int)
newtype Pole left right = Pole { newPole :: (left, right) }

applyLog :: (a,String) -> (a -> (b,String)) -> (b, String)
applyLog' :: (a,[c]) -> (a -> (b,[c])) -> (b,[c]) 

applyLog'' :: (Monoid m) => (a, m) -> (a -> (b,m)) -> (b,m)
applyLog'' (a, c) f = let (b, d) = f a in (b, c mappend d)

newtype Writer w a = Writer { runWriter :: (a, w) } -- order of a and w is inverted!
instance (Monoid w) => Monad (Writer w) where
return x = Writer (x, mempty)
(Writer (x,v)) >>= f = let (Writer(y,v')) = f x in Writer(y, v mappend v')

--To look again
--Record syntax, how objects are created
--Data constructors, e.g Maybe Int, Sum Int, Sum {getSum = 12}..
--Why is monad useful, find real life uses of monad
--Revisit Monad and the walker example using both >>= and do statement!


