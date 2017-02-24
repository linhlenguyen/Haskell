module Main where

merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys) | x<y = add1 x (merge xs (y:ys))
merge (x:xs) (y:ys) = add1 y (merge (x:xs) ys)


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
msort n xs = merge (msort m ys) (msort m zs)
   where m = n `div` 2 - 1
         (ys,zs) = splitAt m xs

          
n = 8
xs = concat (replicate 5 [1..n])

main =
  do { putStrLn ("N = "++show n)
     ; let l = last(msort (length xs) xs)
     ; putStrLn ("The last element of the sort is: "++show l)
     }
