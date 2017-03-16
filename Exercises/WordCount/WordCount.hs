import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Char as Char
--import qualified Data.Set as Set

--charSet :: Set.Set Char
--charSet = Set.fromList "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

isValidChar :: Char -> Bool
isValidChar c = let v = Char.ord c in if (v >= 65 && v <= 90) || (v >= 97 && v <= 122) then True else False

getWords :: String -> [String]
getWords str = reverse $ map reverse $ foldl (\(x:xs) c -> if isValidChar c then (c:x):xs
                                   else if null x then (x:xs)
                                   else []:x:xs) [[]] str

addToMap :: (Ord a) => a -> Map.Map a Int -> Map.Map a Int
addToMap str m = let v = Map.lookup str m in
                 case v of {
                    Nothing -> Map.insert str 1 m;
                    Just c -> Map.insert str (c+1) m;
                 }

toMap :: (Ord a) => [a] -> Map.Map a Int
toMap ls = foldl (\m str -> addToMap str m) Map.empty ls

showMap :: (Show k, Show v) => Map.Map k v -> String
showMap m = Map.foldlWithKey (\a k v -> a ++ "\n" ++ (show v) ++ " " ++ (show k)) "" m

showLs :: (Show k, Show v) => [(k, v)] -> String
showLs [] = []
showLs ((k,v):xs) = show v ++ " " ++ show k ++ "\n" ++ showLs xs

toOrderedList :: Map.Map a Int -> [(a, Int)]
toOrderedList m = List.sortBy (\(ka, va) (kb, vb) -> compare vb va) $ Map.assocs m

getOutputName :: String -> String
getOutputName [] = []
getOutputName (x:xs) = if x == '.' then ".out" else x : getOutputName xs

main :: IO ()
main = do
    command <- getLine
    if command /= "q" then do
      text <- readFile command
      writeFile (getOutputName command) $ showLs $ toOrderedList $ toMap $ getWords text
      main
    else putStrLn "Bye"
