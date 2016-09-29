gcd' :: (Integral a) => a -> a -> a
gcd' x y = let r = mod x y in
          if r == 0 then y else
          gcd y r

--flatList :: Int -> [[Int]] -> [[Int]]
--flatList =

a = [[1,22,34,12,31,1],[2,22,324,112,1,12],[11,22,3,132,31,15]]

--diagonalList :: [[Int]] -> [[Int]]
--diagonalList ((x:xs):ys)

--addCoord :: [a] -> ((Int,Int),[a])
--addCoord x:xs = ()

--flipGrid :: [[Int]] -> [[Int]]

elemAt :: Int -> [a] -> a
elemAt n l = last $ take n l

elemAt' :: Int -> Int -> [[a]] -> a
elemAt' x y l = elemAt x (elemAt y l)

getDiagonalf :: (Int, Int) -> Int -> Int -> [(Int, Int)]
getDiagonalf (x,y) sx sy = if (x == sx) || (y == sy) then (x,y):[] else (x,y) : getDiagonalf (x + 1, y + 1) s

getDiagonalfs :: (Int, Int) -> Int -> [(Int, Int)]
getDiagonalfs t s = getDiagonalf t s s

getDiagonalb :: (Int, Int) -> Int -> Int -> [(Int, Int)]
getDiagonalb (x,y) sx sy = if (x == sx) || (y == sy) then (x,y):[] else (x,y) : getDiagonalb (x - 1, y + 1) s

getDiagonalbs :: (Int, Int) -> Int -> [(Int, Int)]
getDiagonalbs t s = getDiagonalb t s s

--Diagonal :: Int -> (Int, Int)
--Diagonal n = (n,n)
