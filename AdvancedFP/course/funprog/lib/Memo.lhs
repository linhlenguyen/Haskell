> module Memo where

> import IOExts

> memo1 :: (a->b) -> (a->b)
> memo1 f = unsafePerformIO $ do
>   cache <- newIORef []
>   return $ \x -> unsafePerformIO $ do
>               -- print "called"
>               vals <- readIORef cache
>               case x `inCache` vals of
>                 Nothing -> do let y = f x
>                               -- print "writing"
>                               writeIORef cache [(x,y)] -- ((x,y) : 
> --                                if null vals then [] else [head vals])
>                               return y
>                 Just y  -> do -- print "got it"
>                               return y

> inCache :: a -> [(a,b)] -> Maybe b
> x `inCache` [] = Nothing
> x `inCache` ((x',y'):xys) =
>    if unsafePtrEq x x' then Just y'
>    else x `inCache` xys


Test
----

This is OK:

> fib1 n = fibstr !! n
>   where fibstr = 0 : 1 : zipWith (+) fibstr (tail fibstr)

This causes an exponential leak:

> fib2 n = fibstr nil !! n
>   where fibstr = 
>           \x -> 0 : 1 : zipWith (+) (fibstr x) (tail (fibstr x))

And this fixes the leak via memoization:

> fib3 n = fibstr nil !! n
>   where fibstr = memo1 $
>           \x -> 0 : 1 : zipWith (+) (fibstr x) (tail (fibstr x))

> nil = [] :: [Int]

For testing unsafePtrEq:

> ones = 1 : ones
> twos = 2 : twos
> oats = (ones,twos)
> taos = (twos,ones)
