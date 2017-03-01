-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Data.List


--12
--[2,3,4]
--

findUpperBound :: Int -> Int -> Int -> Int
findUpperBound s b x = findBound 0 s b x
    where findBound :: Int -> Int -> Int -> Int -> Int
          findBound i s b x = if (b*x <= s) then findBound (i+1) s (b*x) x else i

findPerm :: Int -> Int -> [Int] -> [[(Bool,Int,Int)]]
findPerm s b [x] = let upperBound = findUpperBound s b x
                       divisible = b*(x^upperBound) == s
                      in [[(divisible, upperBound, x)]]
findPerm s b (x:xs) = let upperBound = findUpperBound s b x
                      in concatMap (\n -> map ((True,n,x):) (findPerm s (b*(x^n)) xs)) [0..upperBound]

filterPerm :: [[(Bool,Int,Int)]] -> [(Int,Int)]
filterPerm xs = let result = filterPerm' xs
                in if null result then [] else head result

filterPerm' :: [[(Bool,Int,Int)]] -> [[(Int,Int)]]
filterPerm' xs = sortBy compareF $ map (\ls -> map (\(b,c,x) -> (c,x)) (filter (\(b,c,x) -> c /= 0) ls)) $ filter (\ls -> not $ any (\(b,c,x) -> b == False) ls) xs

stepCount :: [(Int,Int)] -> Int
stepCount xs = foldr (\(c,n) a -> a + c) 0 xs

compareF :: [(Int,Int)] -> [(Int,Int)] -> Ordering
compareF a b = if (stepCount a) < (stepCount b) then LT
               else if (stepCount a) > (stepCount b) then GT
               else compareF' a b

compareF' :: [(Int, Int)] -> [(Int, Int)] -> Ordering
compareF' [] [] = EQ
compareF' ((c,x):xs) ((c',x'):xs') = if x < x' then LT
                                    else if x > x' then GT
                                    else compareF' xs xs'

toResult :: [(Int, Int)] -> [Int]
toResult [] = []
toResult ((c,x):xs) = replicate c x ++ toResult xs

output :: Int -> [Int] -> [Int]
output i [] = []
output i (x:xs) = (i*x) : (output (i*x) xs)

showLs :: [Int] -> String
showLs [] = []
showLs (x:xs) = show x ++ " " ++ showLs xs

main :: IO ()
main = do
    first <- getLine
    let (s:count:_) = map (read::String -> Int) $ words $ first
    second <- getLine
    let nums = Data.List.sort $ map (read::String -> Int) $ words $ second
    --putStrLn $ show $ filterPerm' $ findPerm s 1 nums
    putStrLn $ showLs $ (\x -> if null x then [-1] else 1:x) $ output 1 $ toResult $ filterPerm $ findPerm s 1 nums
