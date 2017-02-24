-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Control.Monad

splitN :: Int -> [a] -> ([a], [a])
splitN n ls = (reverse $ take n ls, drop (n+1) ls)

filterPrice :: Int -> Int -> [Int] -> [Int]
filterPrice d m [] = []
filterPrice d m (x:xs) = if (x <= (d + m) && x >= d) then x : (filterPrice d m xs) else []

main :: IO ()
main = do
    getLine
    m <- getLine
    let ls = map (read :: String -> Int) $ words m
    n <- getLine
    rs <- forM [1..(read :: String -> Int)n] (\x -> do
        input <- getLine
        let (i:m:_) = map (read :: String -> Int) $ words input
        let d = ls!!i
        let (before, after) = splitN i ls
        --putStrLn $ (show d ++ " " ++ show m)
        --putStrLn $ "lhs" ++ (show before)
        --putStrLn $ "rhs" ++ (show after)
        let count = (length $ filterPrice d m before) + (length $ filterPrice d m after) + 1
        return count)
    mapM_ (\x -> do putStrLn (show x)) rs
