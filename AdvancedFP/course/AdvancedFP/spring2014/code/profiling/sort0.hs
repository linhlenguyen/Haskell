module Main where

merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) | x<y = x : merge xs (y:ys)
merge (x:xs) (y:ys) = y : merge (x:xs) ys


smaller x [] = []
smaller x (y:ys) =
   if x>y
      then y : smaller x ys
      else smaller x ys

larger x [] = []
larger x (y:ys) =
   if x<=y
      then y : larger x ys
      else larger x ys

quik [] = []
quik [x] = [x]
quik (x:xs) = merge (merge small [x]) large
  where small = quik (smaller x xs)
        large = quik (larger x xs)


n = 5000
xs = concat (replicate 5 [1..n])


main =
  do { putStrLn ("N = "++show n)
     ; let l = last(quik xs)
     ; putStrLn ("The last element of the sort is: "++show l)
     }
