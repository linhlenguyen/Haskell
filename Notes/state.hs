--State machine in Haskell

--class Actor a where
--	act :: a -> a -> a

--s -> (a,s)
newtype State s a = State { statef :: s -> (a, s) }

instance Monad (State s) where
return x = State $ \s -> (x,s)
so >>= f = State $ \s -> let (a',s') = (statef so) s
												in statef (f a') s'

--m a -> (a -> m b) -> m b
--State s a -> (a -> State s b) -> State s b


--How to bind states?
--f :: a -> b :: a -> (s -> (a, s))
--a :: m a :: (s -> (a, s))
--b :: m b :: (s -> (b, s))
--Given
--An initial state (s -> (a, s))
--A function f that take a value and produce a state a -> (s -> (a, s))
--We want to be able to apply f to a in initial state and get a new state s -> (b, s)

--(>>=) :: m a -> (a -> m b) -> m b
--(s -> (a, s)) -> (a -> (s -> (b, s))) -> (s -> (b, s)

--si >>= f = State \s -> let (a, s') = statef si s in statef (f a) s'
