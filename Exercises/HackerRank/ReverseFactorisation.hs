-- Enter your code here. Read input from STDIN. Print output to STDOUT

findPerm :: Int -> [Int] -> [[Int]]
findPerm s (x:xs) = undefined

main :: IO ()
main = do
    first <- getLine
    let (s:count:_) = map (read::String -> Int) $ words $ first
    second <- getLine
    let nums = map (read::String -> Int) $ words $ second

    
