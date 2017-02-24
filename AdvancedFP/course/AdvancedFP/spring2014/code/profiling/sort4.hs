module Main where

merge [] ys = ys
merge (x:xs) ys = add1 x (merge xs ys)

add1 x xs = x:xs
add2 x xs = x:xs

smaller x [] = []
smaller x (y:ys) =
   if x>y
      then add2 y (smaller x ys)
      else smaller x ys

larger x [] = []
larger x (y:ys) =
   if x<=y
      then add2 y (larger x ys)
      else larger x ys

quik [] = []
quik [x] = [x]
quik (x:xs) = merge small (x:large)
  where small = quik (smaller x xs)
        large = quik (larger x xs)

msort n [] = []
msort n [x] = [x]
msort n xs = merge2 (msort m ys) (msort (n-m) zs)
   where m = n `div` 2 
         (ys,zs) = splitAt m xs
         merge2 [] xs = xs
         merge2 xs [] = xs
         merge2 (x:xs) (y:ys) | x<y = x : merge2 xs (y:ys)
         merge2 (x:xs) (y:ys) = y : merge2 (x:xs) ys


n = 2500
xs = concat (replicate 5 [1..n])

main =
  do { putStrLn ("N = "++show n)
     ; let l = last(msort (length xs) xs)
     ; putStrLn ("The last element of the sort is: "++show l)
     }
