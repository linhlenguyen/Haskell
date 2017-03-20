import Control.Applicative
import Control.Monad
import System.IO
import Data.Map

type Pole = Int
type Hill = [(Int, [Pole])]

toMap :: [(a, b)] -> Map a [b]
toMap ls = Prelude.foldl (\m (x,w) -> let r = lookup x m in
                              case r of {
                                Nothing -> insert x [w] m;
                                Just v -> insert x (w:v) m;
                              }) empty ls

costCalc :: (Int, [Pole]) -> Int -> Int
costCalc (x, ws) y = Prelude.foldl (\a c -> a + (abs (x - y) * c)) 0 ws 

costLs :: Map Int [Int] -> [Int]
costLs m = Data.Map.foldWithKey (\k x (ls, p) -> if p == -1 then (0:ls,x) else 
                                                 let r = costCalc x k in (r:ls, x)) ([],-1) m

main :: IO ()
main = do
    n_temp <- getLine
    let n_t = words n_temp
    let n = read $ n_t!!0 :: Int
    let k = read $ n_t!!1 :: Int
    hill <- forM [1..n] $ (\a0  -> do
        x_i_temp <- getLine
        let x_i_t = words x_i_temp
        let x_i = read $ x_i_t!!0 :: Int
        let w_i = read $ x_i_t!!1 :: Int
        return (x_i, [w_i]))
    putStrLn $ show hill

getMultipleLines :: Int -> IO [String]

getMultipleLines n
    | n <= 0 = return []
    | otherwise = do          
        x <- getLine         
        xs <- getMultipleLines (n-1)    
        let ret = (x:xs)    
        return ret          

