powerLs :: Int -> [Int]
powerLs n = map (\x -> x^n) [1..]

takeN :: Int -> [Int] -> [Int]
takeN n (x:xs) = if (x <= n) then x : takeN n xs else []

findSum :: Int -> [Int] -> [[(Int, Int)]]
findSum 0 _ = [[(0,0)]]
findSum s [] = [[(-1,0)]]
findSum s (x:xs) = let bound = if (s >= x) then 1 else 0 in filter (\ls -> not $ any (\(r, x) -> r > 1 || r < 0) ls) $ concatMap (\n -> map ((n,x):) (findSum (s - (x*n)) xs)) [0..bound]
--filter (\ls -> not $ any (\(r, x) -> r > 1 || r < 0) ls) $
main :: IO ()
main = do
    s <- getLine
    p <- getLine
    let num = (read::String->Int) s
        power = (read::String->Int) p
        nums = reverse $ takeN num $ powerLs power
    --putStrLn $ show nums
    --putStrLn $ show $ findSum num nums
    putStrLn $ show $ length $ findSum num nums
