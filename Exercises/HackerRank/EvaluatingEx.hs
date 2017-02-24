import Control.Applicative
import Control.Monad
import System.IO

fac :: Int -> Double
fac n = fromIntegral $ foldr (*) 1 [1..n]

series :: Double -> [Double]
series x = 1 : beginSeries x 1
    where beginSeries :: Double -> Int -> [Double]
          beginSeries x n = x^n/(fac n) : beginSeries x (n+1)

computeEx :: Double -> Double
computeEx x = foldr (+) 0 $ take 10 (series x)

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    forM_ [1..n] $ \a0  -> do
        x_temp <- getLine
        let x = read x_temp :: Double
        putStrLn $ show (computeEx x)

getMultipleLines :: Int -> IO [String]

getMultipleLines n
    | n <= 0 = return []
    | otherwise = do
        x <- getLine
        xs <- getMultipleLines (n-1)
        let ret = (x:xs)
        return ret
