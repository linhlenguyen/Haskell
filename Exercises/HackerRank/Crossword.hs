-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Control.Monad
import Data.List

type Board = [String]
data Direction = Horizontal | Vertical deriving (Show)

solve :: (Board,[String]) -> [(Board,[String])]
solve r@(board,strs) = let d = findDash board
                        in case d of {
                           Nothing -> [r];
                           Just p@(x,y) -> let hs:vs:_ = getWords p board
                                               (hl:hm:hr) = getCharLsFrom x vs
                                               (vu:vm:vl) = getCharLsFrom y vs
                                               hmatches = matching hm strs
                                               vmatches = matching vm strs
                                               hboard = map (\(s, xs) -> (fillWith Horizontal (x,y) s board, xs)) hmatches
                                               vboard = map (\(s, xs) -> (fillWith Vertical (x,y) s board, xs)) hmatches
                                            in (concatMap solve hboard) ++ (concatMap solve vboard);
                          }

matching :: String -> [String] -> [(String, [String])]
matching s xs = filter(\(x,_) -> not $ null x) $ map (\x -> if isMatch x s then (x,delete x xs) else ([],[])) xs

fillWith :: Direction -> (Int, Int) -> String -> Board -> Board
fillWith Vertical (x,y) str b = let tuples = zip b str in
                                 replace' tuples y
fillWith Horizontal (x,y) str b = replace b x str

replace' :: [([a],a)] -> Int -> [[a]]
replace' ls i = map (\(xs,x) -> replace xs i x) ls

replace :: [a] -> Int -> a -> [a]
replace ls i a = (take i ls) ++ a:(drop (i+1) ls)

getVertical :: Int -> Board -> String
getVertical y b = map (\x -> x!!y) b

getWords :: (Int, Int) -> Board -> [String]
getWords (x,y) b = let horizontal = b!!x
                       vertical = getVertical y b
                   in [horizontal, vertical]

filterLs :: (Eq a) => [a] -> a -> [a]
filterLs ls a = filter (\x -> x /= a) ls

isMatch :: String -> String -> Bool
isMatch [] [] = True
isMatch [] _ = True
isMatch _ [] = False
isMatch l1@(x:xs) l2@(y:ys) = if length l1 /= length l2 then False
                              else if (x == y || x == '-' || y == '-') then isMatch xs ys else False

getCharLsFrom :: Int -> String -> [String]
getCharLsFrom index str = let li = (lhs index str)
                              ri = (rhs index str)
                          in [take (li + 1) str, take (ri - li - 1) $ drop (li + 1) str, drop ri str]
  where lhs i s = if i >= 0 && s!!i /= '+' then (lhs (i-1) s) else i
        rhs i s = if i < (length s) && s!!i /= '+' then (rhs (i+1) s) else i

findDash :: Board -> Maybe (Int, Int)
findDash board = beginF (0,0) board
    where beginF (x,y) b = if (b!!y)!!x == '-' then Just (x,y)
                     else if (x+1) < length (b!!0) then beginF (x + 1, y) b
                     else if (y+1) < length b then beginF (0, y + 1) b
                     else Nothing

main :: IO ()
main = do
    board <- Control.Monad.forM [0..9] (\x -> do
        n <- getLine
        return n)
    --let board = fromList $ Prelude.map fromList b
    w <- getLine
    let ws = Prelude.map Prelude.reverse $ Prelude.foldl (\ls@(x:xs) c -> if c == ';' then []:ls else (c:x):xs) [[]] w
    Control.Monad.forM_ board (\x ->
        putStrLn $ show x)
    putStrLn $ show ws

toLines :: String -> String -> [String]
toLines ls str = reverse $ map reverse $ foldl (\(a:as) x -> if any(\c -> c == x)ls then []:(a:as) else ((x:a):as)) [[]] str

mainF :: IO ()
mainF = do
    let fin = "Crossword.in"
    text <- readFile fin
    let input = init $ toLines "\n" text
    let board = take 10 $ input
    let strs = toLines ";" $ last input
    putStrLn $ show board
    putStrLn $ show $ strs
    --putStrLn $ show $ findDash board
    putStrLn $ show $ solve (board,strs)
