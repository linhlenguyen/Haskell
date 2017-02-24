module Main where

mean :: [Float] -> Float
mean xs = loop 0 0 xs where
  loop sum len [] = sum / len
  loop sum len (x:xs) = 
                        loop (sum+x) (len+1) xs

main = print (mean [0.0 .. 1000000])
