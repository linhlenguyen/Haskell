module ShortestDistance()
where
  -- You have a large text file containing words. Given any two words, find the
  -- shortest distance (in terms of number of words) between them in the file. If the
  -- operation will be repeated many times for the same file (but different pairs of
  -- words), can you optimize your solution?

  --Store the text file as string index map
  --TextFile :: Map String Index
  --To find minimum distance between 2 words,
  --Lookup the indexes in the map and get the shortest distance from 2 index lists

  import qualified Data.Map as Map

  toStrings :: String -> [String]
  toStrings ls = foldl foldf [] ls
    where foldf :: [String] -> Char -> [String]
          foldf = undefined

  toMap :: [String] -> Map.Map String [Int]
  toMap ls = fromlist $ snd $ foldl foldf (0,[]) ls
    where foldf :: (Int,[(String, [Int])]) -> String -> (Int,[(String, [Int])])
          foldf (index, ls) = undefined

  --brute force
  shortestDistance :: [Int] -> [Int] -> Int
  shortestDistance xs ys = let fs = (map (\x -> (subtract x)) xs)
    in minimum $ concatMap (\y -> map (\f -> abs $ f y) fs) ys

  --reduceLists :: ([Int],[Int]) -> ([Int],[Int])
  --reduceLists (xs,ys) =
