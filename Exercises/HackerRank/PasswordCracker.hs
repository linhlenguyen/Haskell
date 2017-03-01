import Control.Monad

hasPrefix :: (Eq a) => [a] -> [a] -> Bool
hasPrefix [] [] = True
hasPrefix _ [] = False
hasPrefix [] _ = True
hasPrefix (x:xs) (y:ys) = if (x == y) then hasPrefix xs ys else False

hasAnyMatch :: (Eq a) => [a] -> [[a]] -> [[(Bool,[a])]]
hasAnyMatch [] _ = [[(True,[])]]
hasAnyMatch str xs = filter (\ls -> not $ any (\(b, str) -> b == False) ls) $ concatMap (\x -> if hasPrefix x str
									  then (map ((True,x):) (hasAnyMatch (drop (length x) str) xs))
									  else [[(False,[])]]) xs

processR :: [(Bool,[Char])] -> [Char]
processR [] = []
processR ((b,x):xs) = x ++ " " ++ processR xs

main :: IO ()
main = do
    n <- getLine
    forM_ [1..(read::String -> Int)n] (\x -> do
        m <- getLine
        p <- getLine
        let keywords = words p
        password <- getLine
        let results = hasAnyMatch password keywords
        let output = if null results then "WRONG PASSWORD" else processR $ head $ results
        putStrLn output
        )
