module DemoPrime where

import Colour
import PPM6


prime x = p x 2 (x `div` 2 + 1)
  where p x next limit 
          | next >= limit = True
          | x `mod` next == 0 = False
          | True = p x (next+1) limit


darkPurple = Colour 0.25 0  0.25

pcol x = if prime x then white else darkPurple 

main = mapPixel "qprimes.ppm" (\ i j -> pcol ((i-1)*100 + j)) 100 100
test = mapPixel "qprimes2.ppm" (\ i j -> pcol ((i-1)*101 + j)) 101 101
