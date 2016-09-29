--Monad

class Monad m where
return :: a -> m a
(>>=) :: m a -> (a -> m b) -> m b
(>>) :: m a -> m b -> m b
x >> y = x >>= \_ -> y -- default implementation

instance Monad Maybe where
return a = Just a
Just a >>= f = f a
Nothing >>= _ = Nothing

instance Monad [] where
return a = [a]
(>>=) :: [a] -> (a -> [b]) -> [b]
x:xs >>= f = f x ++ xs >>= f
xs >>= f = concat (map f xs)

data Maybe a = Nothing | Just a

return (0,0) >>= landLeft 2 >>= landRight 2 >>= landLeft 5 >>= landLeft 1
return (0,0) :: Maybe Pole

landLeft x (left,right) 
		| abs((left + x) - right) < 4 = Just (left + x,right)
		| otherwise = Nothing

Just (0,0) >>= landLeft 2 
m a -> (\a -> m b)
		
--chaining Monadic values
--do notation 
							
do 
start <- return (0,0)
first <- landLeft 2 start
second <- landRight 2 first
third <- landLeft 5 second
landLeft 1 third

instance Monad ((->) r) where
return a = \_ -> a
m a -> (a -> m b) -> m b
(r -> a) -> (a -> (r -> b)) -> (r -> b)
f >>= g = \x -> g (f x) x

addStuff :: Int -> Int
addStuff = do 
	a <- (*2)
	b <- (+10)
	return (a + b)
	
addStuff :: Int -> Int
addStuff x = do 
	a <- (*2) x
	b <- (+10) x
	return (a + b)

(\a -> a * 2) >>= (+)
(\a -> a * 2 + a)
(\a -> a + 10) >>= (\a -> a * 2 + a)
(\a -> (a + 10) * 2 + (a + 10))

(*2) >>= (\x -> x+10) >>= 
--What was I doing?

--The state Monad!
--Computation with context

--Stack as list
pop :: [a] -> a
pop (x:xs) = x
push :: a -> [a] -> [a]
push x xs = x : xs

--Stack with context
newtype Stack a = [a]
pop :: Stack -> (a, Stack)
pop (x:xs) = (x,xs)
push :: a -> Stack -> ((),Stack)
push a x = ((), a : x)

s -> ( a , s )
newtype State s a = State { runstate :: s -> (a , s) }
pop :: Stack -> (a, Stack)
--hence pop can be applied to State constructor
State pop
--or rewritten as
pop :: State Stack a
-- State s a is essentially s -> (s, a)
(>>=) :: m a -> (a -> m b) -> m b
(>>=) :: State s a -> (a -> State s b) -> State s b
(>>=) :: s -> (s, a) -> a -> (s -> (s, b)) -> s -> (s, b)

instance Monad (State s) where
return x = State \s -> (x,s)
ms >>= f = State \s -> let (a, s') = runState ms s
						in runState (f a) s'

f :: b -> (s -> (a,s))
						
stateFunction :: State [a] () :: [a] -> ((),[a])
stateFunction = do 
				x <- pop
				pop
				push x
				
-- de-sugar to become m >>= \a -> f .. or ( m >>= \_ -> f )
-- changing between do syntax and >>=



m a >>= (\a -> m b) >>= (\b -> m c) >>= (\c -> m c)
x <- f1 m
y <- f2 x
z <- f3 z
return z

landLeft :: Int -> (Int, Int) -> (Int,Int)
landLeft a (b,c) = (a + b, c)
landRight :: Int -> (Int, Int) -> (Int, Int)
landRight a (b,c) = (a, b + c)

landRight 2 landLeft 3 landRight 1 landLeft 0 (0,0)
(0,0) -: landLeft 0 -: landRight 1 -: landLeft 3 -: landRight 2

landLeft :: Int -> Maybe (Int,Int) -> Maybe (Int, Int)
landLeft a Nothing = Nothing
landLeft a Just (b,c)  
					| abs(a+b - c) > 4 = Nothing
					| otherwise = Just (a+b,c)

return (0,0) >>= landLeft 4 >>= landRight 5

foo :: Maybe String  
foo = Just 3   >>= (\x -> 
      Just "!" >>= (\y -> 
      Just (show x ++ y)))  
	  
foo :: Maybe String  
foo = do  
    x <- Just 3  
    y <- Just "!"  
    Just (show x ++ y)  
	
filterM :: (Monad m) => (a -> m Bool) -> [a] -> m [a]

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show
instance Functor Prob where
fmap f p = Prob $ map (\(x,s) -> (f x, s)) getProb p

class Testable a where
test :: a -> RandSupply -> Bool

class Arbitrary a where
arby :: RandSupply -> a

instance Testable Bool where
test b r = b

instance (Arbitrary a, Testable b) 
=> Testable (a->b) where
test f r = test (f arby r1) r2 where (r1,r2) = split r

(split) :: RandSupply -> (RandSupply, RandSupply)

lift :: (a -> b) -> m a -> m b
liftMaybe :: (a -> b) -> Maybe a -> Maybe be

liftMaybe f Nothing _ = Nothing
liftMaybe f _ Nothing = Nothing
liftMaybe f Just a Just b = Just f a b

liftM2 :: (a -> b -> c) -> m a -> m b -> m c
liftM2 f m1 m2 = do
		x1 <- m1 
		x2 <- m2
		return f (x1, x2) --boxing done here
		
--do is only a syntatic sugar 	
liftM2 f m1 m2 = m1 >>= (\x1 -> m2 >>= (\x2 -> return (f (x1,x2))))
	