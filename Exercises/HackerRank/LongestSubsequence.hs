findA :: (Ord a) => a -> [a] -> [[a]] -> [a]
findA a l [] = l
findA a l (ls:lss) = if ((head ls) <= a && length l < length ls + 1)
                      then findA a (a:ls) lss
                      else findA a l lss

longestSubsequence :: (Ord a) => [a] -> [[a]]
longestSubsequence ls = foldl (\a n -> (findA n [n] a):a) [] ls

longestLs :: [a] -> [[a]] -> [a]
longestLs a [] = a
longestLs [] (x:xs) = longestLs x xs
longestLs a (x:xs) = if length a < length x then longestLs x xs else longestLs a xs

mostDifference :: (Ord a, Num a) => a -> [[a]] -> a
mostDifference a [] = a
mostDifference a (x:xs) = let diff = abs $ (head x) - (last x) in
                          if diff > 0 && diff > a then mostDifference diff xs
                          else mostDifference a xs

main :: IO ()
main = do
  n <- getLine
  let nums = map (read::String -> Int) $ words n
  putStrLn $ show $ mostDifference (-1) $ longestSubsequence nums
