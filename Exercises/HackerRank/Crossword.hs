-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Control.Monad

main :: IO ()
main = do
    a <- forM [0..10] (\x -> do
        n <- getLine
        return n)
    forM_ a (\x ->
        putStrLn x)
