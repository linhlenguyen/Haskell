-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Control.Monad

rows = 63
columns = 100

n = [16,8,4,2,1]
m = [1,2,4,8,16]

r0 :: [[Char]]
r0 = replicate rows $ replicate columns '_'

i0 :: [[Int]]
i0 = (replicate (n!!0) [49::Int]) ++ (stepN (n!!0) [49::Int])

indexes :: Int -> [[Int]]
indexes iteration = i0 ++ (beginN (last i0) 1 iteration)
  where beginN :: [Int] -> Int -> Int -> [[Int]]
        beginN xs i iteration = if i >= iteration then [] else
                                let r = (replicate (n!!i) xs) ++ (stepN (n!!i) xs) in r ++ (beginN (last r) (i+1) iteration)

-- [50] -> [[49,51], [48,52], [47,53], ..]
stepN :: Int -> [Int] -> [[Int]]
stepN n x = foldr (\c a -> (concatMap(\x -> [x-c,x+c]) x):a) [] [1..n]

rn :: [[Char]] -> [[Int]] -> [[Char]]
rn c [] = c
rn c@(x:xs) (y:ys) = (replace x y) : (rn xs ys)

replace :: [Char] -> [Int] -> [Char]
replace str indices = beginReplace 0 str indices
  where beginReplace :: Int -> [Char] -> [Int] -> [Char]
        beginReplace _ [] _ = []
        beginReplace c (x:xs) indices = if any (\x -> x == c) indices
                                        then '1' : (beginReplace (c+1) xs indices)
                                        else x : (beginReplace (c+1) xs indices)

generateTree :: Int -> [[Char]]
generateTree n = reverse $ rn r0 (indexes n)

main :: IO ()
main = do
    n <- getLine
    let num = (read::String->Int)n
    forM_ [0..(rows-1)] (\x -> do
        putStrLn $ (generateTree num)!!x)
