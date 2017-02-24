rotate :: [a] -> [a]
rotate (x:xs) = xs ++ [x]

rotateN :: [a] -> [[a]]
rotateN ls = ls : beginRotate ls
    where beginRotate :: [a] -> [[a]]
          beginRotate ls = rotate ls : beginRotate (rotate ls)

f :: Int -> [Int] -> [Int]
f n [] = []
f n (x:xs) = replicate n x ++ f n xs

-- This part handles the Input and Output and can be used as it is. Do not modify this part.
main :: IO ()
main = getContents >>=
       mapM_ print. (\(n:arr) -> f n arr). map read. words

test :: IO ()
test = getLine >>= (\a -> getLine >>= (\b -> putStrLn (a ++ b)))


test2 :: IO ()
test2 = getLine >>= (\a -> getLine) >>= (\b -> putStrLn b)
