-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Control.Monad
import Data.Vector

type Board = [[Char]]

solve :: Board -> [String] -> Maybe Board
solve b = let d = findDash b in
          case d of {
            Nothing -> Just b
            Just (x,y) ->
          }

solveAt :: (Int, Int) -> (Board,[String]) -> Maybe [(Board, [String])]
solveAt p (b,ls) = let h:v:_ = getWords p b
                       r = filter (\x -> x /= Nothing) $ map (tryFill ls) w
                   in

updateBoard :: Board ->

getVertical :: Int -> Board -> String
getVertical y b = map (\x -> x!!y) b

trim :: String -> String
trim v = beginTrim 0 v True
   where beginTrim i v isLHS = if v!!i /= '+' && isLHS then v!!i : (beginTrim (i+1) v False) else (beginTrim (i+1) v isLHS)

getWords :: (Int, Int) -> Board -> [[String]]
getWords (x,y) b = let horizontal = trim $ b!!x
                       vertical = trim $ getVertical y b 
                   in [horizontal, vertical]

wordsAt :: (Int, Int) -> Board -> [[String]]
wordsAt (x,y) b = [vertical (x,y) b, horizontal (x,y) b]
  where vertical (x, y) b =

tryFill :: [String] -> String -> Maybe [String]
tryFill xs s = let r = let r = concatMap (\x -> if isMatch x s then [x] else []) xs in
                       case r of {
                           [] -> Nothing;
                           a -> Just a;
                       }

filterLs :: (Eq a) => [a] -> a -> [a]
filterLs ls a = filter (\x -> x /= a) ls

isMatch :: String -> String -> Bool
isMatch [] [] = True
isMatch _ [] = False
isMatch [] _ = False
isMatch l1@(x:xs) l2@(y:ys) = if (x == '_' || y == '_' || x == y) then isMatch xs ys else False

findDash :: Board -> Maybe (Int, Int)
findDash b = beginF (0,0) b
    where beginF (x,y) b = if (b!!x)!!y == '_' then Just (x,y)
                     else if x <= width then beginF (x + 1, y) b
                     else if y <= height then beginF (0, y + 1) b
                     else Nothing

width = 9::Int
height = 9::Int

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
