module Main where

mean :: [Float] -> Float
mean xs = sum xs / 
     (fromIntegral (length xs))

main = print (mean [0.0 .. 1000000])