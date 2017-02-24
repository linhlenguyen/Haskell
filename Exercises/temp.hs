fa 0 = 1
fa n = n * fa (n - 1)

nCr n r = div (fa n) (fa (n-r) * fa r)

reverse' :: [a] -> [a]
reverse' ls = reverse'' ls []
  where reverse'' :: [a] -> [a] -> [a]
        reverse'' [] a = a
        reverse'' (x:xs) a = reverse'' xs (x:a)

fibn :: Int -> Int
fibn n = last $ take n fibl
  where fibl :: [Int]
        fibl = 0 : 1 : (beginFib 0 1)
        beginFib :: Int -> Int -> [Int]
        beginFib x y = (x+y) : (beginFib y (x+y))

primen :: Int -> [Int]
primen n = primes n
  where primes :: Int -> [Int]
        primes n = foldl foldf [] [1..n]
        foldf :: [Int] -> Int -> [Int]
        foldf xs a = if (length (filter (\x -> mod a x == 0) xs) <= 1) then a:xs else xs

primen' :: [Int]
primen' = [x | x <- [1..], length (filter (\a -> mod x a == 0) [1..x]) <= 2]
