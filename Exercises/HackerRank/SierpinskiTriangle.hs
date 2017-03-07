-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Control.Monad

rows = 32
columns = 63

heights = [64, 32, 16, 8, 4, 2, 1]

firstTip = 31::Int

--[[t1]] -> [[t1] [t2 t3]] -> [[t1] [t4 t5] [t2 t3] [t6 t7 t8 t9]]
--[t1] [t10 t11] [t4 t5] [t12 t13 t14 t15] [t2 t3] [t6 t7 t8 t9]

newTip :: Int -> [Int] -> [[Int]]
newTip height x = [x,ls] where
  ls = concatMap (\a -> [(a - height), (a + height)]) x

triangleExpansion :: [[[Int]]]
triangleExpansion = [[firstTip]] : beginExpansion [[firstTip]] 2
    where beginExpansion :: [[Int]] -> Int -> [[[Int]]]
          beginExpansion ls n = let newLs = concatMap (newTip (heights!!n)) ls in newLs : beginExpansion newLs (n+1)

fullTriangles :: [[Int]] -> Int -> [[Int]]
fullTriangles xs height = concatMap (generateTriangle height) xs

generateTriangle :: Int -> [Int] -> [[Int]]
generateTriangle height ls = ls : beginGenerate 1 ls
  where beginGenerate :: Int -> [Int] -> [[Int]]
        beginGenerate i l = if i >= height then [] else let newLs = concatMap (\x -> [x-i..x+i]) l in newLs : beginGenerate (i+1) l

generate :: Int -> [[Int]]
generate step = let triangles = last $ take step $ triangleExpansion in fullTriangles triangles (heights!!step)

draw :: [[Int]] -> [[Char]]
draw ls = let first = replicate rows $ replicate columns '_' in beginReplace first ls
  where beginReplace :: [[Char]] -> [[Int]] -> [[Char]]
        beginReplace _ [] = []
        beginReplace [] _ = []
        beginReplace (c:cs) (x:xs) = replace c x : beginReplace cs xs

replace :: [Char] -> [Int] -> [Char]
replace cs ls = fst $ foldl (\(a,i) c -> if any (\x -> x == i) ls then (('1':a), i+1) else (('_':a), i+1)) ([], 0) cs

main :: IO ()
main = do
  n <- getLine
  let r = draw $ generate $ ((read::String -> Int) n)+1
  forM_ [0..((length r)-1)] (\x -> do
    putStrLn (r!!x))
