module DemoPrime where

import Colour
import PPM6


prime x = p x 2 (x `div` 2 + 1)
  where p x next limit 
          | next >= limit = True
          | x `mod` next == 0 = False
          | True = p x (next+1) limit
          
pcol x = if prime x then white else black  


main = quick_ppm "qprimes.ppm" (\ i j -> pcol ((i-1)*200 + j)) 100 100
