module LazyDemos where

import Control.Monad.State hiding(fix)
import Data.Array

get,inc:: State Int Int
inc = mapState f (return ())
  where f ((),s) = (s,s+1)
get = mapState f (return ())
  where f ((),s) = (s,s)


-------------------------------
-- Cyclic lists

cycles:: [Int]
cycles = 1 : 2 : 3 : cycles

namedCycles = evalState (mapM f cycles) 0
  where f x = do { n <- inc; return(x,n)}

-- Cyclic trees

data Tree a = Tip a | Fork (Tree a) (Tree a)

t2 = Fork (Fork (Tip 3) (Tip 4)) (Fork (Tip 9) t2)

-- Mutually cyclic

t3,t4:: Tree Int
(t3,t4) = ( Fork (Fork (Tip 11) t3) t4
          , Fork (Tip 21) (Fork (Tip 33) t3)
          )
-----------------------------------------
-- primes

primes :: [Integer]
primes = sieve [2..]
  where sieve (p:xs) = p : sieve [x | x<-xs, x `mod` p /= 0]


-------------------------------------
-- Lazy arrays

table = array (1,5) [(1,'a'),(2,'b'),(3,'c'),(5,'e'),(4,'d')]

-------------------------------------
-- Dynamic programming


fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)


fix f = f (fix f)

g fib 0 = 1
g fib 1 = 1
g fib n = fib (n-1) + fib (n-2)

fib1 = fix g

fib2 :: Integer -> Integer
fib2 z = f z
  where table = array (0,z) [ (i, f i) | i <- range (0,z) ]
        f 0 = 1
        f 1 = 1
        f n = (table ! (n-1)) + (table ! (n-2))
