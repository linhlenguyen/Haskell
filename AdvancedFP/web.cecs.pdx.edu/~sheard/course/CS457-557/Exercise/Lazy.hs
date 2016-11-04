module Lazy where
import Test.QuickCheck
import Ratio

-- ones is an infinite list of ones. i.e. [1,1,1,1,...]
ones:: [Int]
ones = 1 : ones

prop_ones n = all (==1) (take n ones)

-- from n is an infinite list of ascending numbers starting at n
from:: Int -> [Int]
from n = n : from (n+1)

prop_from n m = ok (take n (from m))
  where ok [] = True
        ok [x] = True
        ok (x:y:zs) = (x+1)==y && ok (y:zs)

-- Another way to compute an infinite list of ascending unmbers
from2 = 2 : (map (+1) from2)

-- Define a property for quickcheck to test for from2
prop_from2 = undefined

-- Use the technique of from2 to create the list [1, 1/2, 1/4, 1/8, 1/16, ...]
-- You will need to use the operator (%) that builds ratios.  (1 % 4) --> 1/4
-- Two important ratios are:

one :: Ratio Integer
one = 1

half :: Ratio Integer
half = 1 % 2

converge :: [Ratio Integer]
converge = one : (map (* half) converge)

-- write test that shows the sum of the first n terms of 
-- converge is always less than 2

prop_converge n = sum (take n converge) < (2:: Ratio Integer)

