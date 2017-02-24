import Control.Monad
import Data.List

hasDuplicate :: (Eq a) => [a] -> Bool
hasDuplicate [] = False
hasDuplicate [x] = False
hasDuplicate [x,y] = x == y
hasDuplicate (x:y:xs) = if x == y then True else hasDuplicate (y:xs)

main :: IO ()
main = do
    n <- getLine
    forM_ [1.. (read :: String -> Int) n] (\testCases -> do
        m <- getLine
        ls <- forM [1.. (read :: String -> Int) m] (\pairs -> do
          ln <- getLine
          let vPairs = map (read :: String -> Int) $ words ln
          return vPairs)
        let domainValues = map (head) ls
        putStrLn (if hasDuplicate (sort domainValues) then "NO" else "YES"))
