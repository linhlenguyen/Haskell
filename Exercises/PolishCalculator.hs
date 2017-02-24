--Polish notation calculator
--"10  3 + 2 * -"

pncf :: (Num a) => [a] -> String -> [a]
pncf [x] _ = [x]
pncf (x:y:xs) "+" = (x + y) : xs
pncf (x:y:xs) "-" = (x - y) : xs
pncf (x:y:xs) "*" = (x * y) : xs
pncf (x:y:xs) "/" = (x / y) : xs
pncf x n = read n:x

solveRPN :: (Num a, Read a) => String -> a
solveRPN = head. fold f [] . words
  where f (x:y:ys) "*" = (x*y):ys
        f (x:y:ys) "+" = (x+y):ys
        f (x:y:ys) "-" = (x-y):ys
        f (x:y:ys) "/" = (x/y):ys
        f x n = read n:x

toTenary :: [a] -> [(a,a,a)]
toTenary

newP :: (Num a) => (a,a) -> (a,a,a) -> (a,a)
newP (s1,s2) (p1,p2,p3) = let w1 = s1 + p1
                              w2 = s2 + p2
                          in (if w1 + p3 < w2 then w1 + p3 else w2,
                              if w2 + p3 < w1 then w2 + p3 else w1)

pathF :: (Num a) => [(a,a)] -> [a] -> [(a,a)]
pathF lp x:y:z:[] = let (p1,p2) = tail lp
                    in newP
