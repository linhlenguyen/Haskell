add1 :: (Num a) => a -> [a] -> [a]
add1 _ [] = []
add1 c (x:xs) = (x + c) : add1 (c + 1) xs

count [] = 0
count (x:xs) = 1 + count xs

apply [] _ = []
apply (f:fs) x = concat (map f x : apply fs x : [])

ltlt' :: Int -> [a] -> [(Int, a)]
ltlt' _ [] = []
ltlt' c (x:xs) = (c,x) : ltlt' (c + 1) xs

ltlt :: [a] -> [(Int, a)]
ltlt = ltlt' 0

data State s a = { runState :: (a, s) }
