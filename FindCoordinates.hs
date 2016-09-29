--Plot f 2D
import Control.Monad

newtype State s a = State { statef :: s -> (a,s) }
instance Monad (State s) where
	return a = State $ \s -> (a,s)
	si >>= f = State $ \s ->
			let (a,s') = statef si s -- initial state and a
			in statef (f a) s' -- f a and new state

--(a -> m b)
stepf :: (Float -> Float) -> Float -> State Float (Float,Float)
stepf f s = State $ \x -> ((x, f x),x + s)

--For every step of the equation, plot y, increment x by step
plotf :: (Float -> Float) -> Float -> State Float [(Float,Float)]
plotf f s = do
		a <- stepf f s
		b <- plotf f s
		return (a : b)

plotf' :: (Float -> Float) -> Float -> State Float [(Float,Float)]
plotf' f s = stepf f s >>= (\a -> plotf' f s >>= (\b -> return (a : b)))

-- State $ \s0 -> statef ((\a -> plotf' f s >>= (\b -> return (a : [b]))) (s0, f s0)) (s0 + s)
-- State $ \s0 -> statef ( plotf' f s >>= (\b -> return ((s0, f s0) : [b]))) (s0 + s)

-- State s a >>= (\a -> (State s [a]) >>= (\b -> State s [a]))

-- Recursive Monad
-- r :: m [a]
-- r do
-- a <- m a
-- b <- r
-- return (a : b)

-- m a >>= (\a -> r >>= (\b -> m (a : b))

-- r ~ m b >>= (\b -> m (b : []))
-- r ~ m (b : [])

-- m a >>= (\a -> m (b : []) >>= (\b -> m (a : b))
-- m a >>= (\a -> m (a : b : []))
-- m (a : b : [])

plotFTwice f s = do
		a <- stepf f s
		stepf f s
		stepf f s
		return a

plotFNoSugar :: (Float -> Float) -> Float -> State Float [(Float,Float)]
plotFNoSugar f s = stepf f s >>= (\a -> stepf f s >>= (\b -> return (a : [b])))

stepTest f s = stepf f s >>= (\x -> State $ \y -> (x,y))

--State $ \s0 -> let (a,s1) = statef (State $ \x -> ((x, f x),x + s)) s0
--				 in statef (\x -> State $ \y -> (x,y)) a s1

--State $ \s0 -> let (a,s1) = ((s0, f s0),s0 + s)
--				in statef (\x -> State $ \y -> (x,y)) a s1

--State $ \s0 -> statef ((\x -> State $ \y -> (x,y)) (s0, f s0)) (s0 + s)

--State $ \s0 -> statef (State $ \y -> ((s0, f s0),y)) (s0 + s)

--State $ \s0 -> ((s0, f s0),(s0 + s))e

testFunc :: Float -> Float
testFunc x = 9*x*x*x - 8*x*x + 20*x -100

testFunc' :: Float -> Float
testFunc' x = x*x

f :: Float -> Float
f x = x*x

-- State-less implementation
plotF' :: (RealFloat a) => (a -> a) -> a -> a -> [(a,a)]
plotF' fnc x0 step = (x0, fnc x0) : plotF' fnc (x0 + step) step

-- (\s -> (a, s)) >>= \a ->
-- (\s -> (b, s')) >>= \b ->
-- (\s -> (a:b,s))

-- de-sugaring
-- m a -> ( a -> m b ) -> m b
-- m a -> (\a -> m b )
-- m a >>= (\s' ->  )

-- (\s -> (a, s)) >>= \a ->
-- (\s -> (b, s')) >>= \b ->
-- ...
-- State $ \s -> (a : b)

-- extract (a,s) using given state si
-- apply f to a gives \s -> (b,s')
-- extract (b,s') by running statef (\s -> (b,s')) s
-- \s -> (b,s') >>= \b -> ...

-- s0
-- ((s0, f s0), s0 + s) ~ (a1, s1)
-- ((s1, f s1), s1 + s) ~ (a2, s2)
-- ...
-- [a1,a2...] sn

-- ST a >>= (\a -> ST b >>= (\b -> ST (a : b)))
-- ST a >>= (\a -> ST (a : b))
-- ST (a : b)
-- \s -> ((a : b),s)

-- m a >>= (\a -> m b >>= (\b -> return (a + b)))
-- m a >>= (\a -> m (a + b))
-- m (a + b)

-- (s' -> stepF fnc
-- s' : s'' [State Float (Float,Float)]

-- \s -> ([State Float (Float,Float)],s)
