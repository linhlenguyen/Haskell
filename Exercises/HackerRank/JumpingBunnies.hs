-- Enter your code here. Read input from STDIN. Print output to
import Data.List

divisible :: Int -> [Int] -> Bool
divisible n [] = True
divisible n (x:xs) = if (mod n x) /= 0 then False else divisible n xs

gcd' :: [Int] -> Int
gcd' xs = head $ filter (\x -> (divisible x xs)) [(last xs),(last xs)+(last xs)..]

main :: IO ()
main = do
    n <- getLine
    m <- getLine
    let nums = sort $ map (read::String ->Int) $ words m
    putStrLn $ show $ gcd' nums
