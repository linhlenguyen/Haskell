-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Control.Monad
import qualified Data.Vector as V

countN :: (Int -> Int) -> Int -> Int -> Int -> V.Vector Int -> Int
countN f d m i v = if i < 0 || i >= (V.length v) then 0 else let x = (V.!) v i in if (x <= (d + m) && x >= d) then 1 + countN f d m (f i) v else 0

main :: IO ()
main = do
    getLine
    m <- getLine
    let v = V.fromList $ map (read :: String -> Int) $ words m
    n <- getLine
    rs <- forM [1..(read :: String -> Int)n] (\x -> do
        input <- getLine
        let (i:m:_) = map (read :: String -> Int) $ words input
        let d = (V.!) v i
        --let (before, after) = V.splitAt i v
        --putStrLn $ (show d ++ " " ++ show m)
        --putStrLn $ "lhs" ++ (show before)
        --putStrLn $ "rhs" ++ (show after)
        let inc = (\x -> x + 1)
        let dec = (\x -> x - 1)
        let count = (countN dec d m (dec i) v) + (countN inc d m (inc i) v) + 1
        return count)
    mapM_ (\x -> do putStrLn (show x)) rs
