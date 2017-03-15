import Control.Monad

data Direction = R | D deriving (Show)

type Sides = (Int, Int, Int)

rotate :: Direction -> Sides -> Sides
rotate R (s1, s2, s3) = (7 - s3, s2, s1)
rotate D (s1, s2, s3) = (7 - s2, s1, s3)

allPaths :: (Int, Int) -> (Int, Int) -> [[Direction]]
allPaths (x,y) (dx,dy) = if (x == dx && y == dy) then []
                         else if (x == dx) then [replicate (dy - y) R]
                         else if (y == dy) then [replicate (dx - x) D]
                         else (map (R:) (allPaths (x, y+1) (dx,dy))) ++ (map (D:) (allPaths (x+1, y) (dx,dy)))

sumPath :: [Direction] -> Int
sumPath ps = fst $ foldl (\(t,s) d -> let r@(r1,r2,r3) = (rotate d s) in (t+r1,r)) (1,(1,2,4)) ps

minPath :: (Int, Int) -> Int
minPath (dx, dy) = let paths = (allPaths (1,1) (dx,dy)) in if null paths then 1 else (maximum $ map (sumPath) paths)

main :: IO ()
main = do
    n <- getLine
    forM_ [0..(read::String ->Int)n] (\x -> do
        m <- getLine
        let x:y:_ = map (read::String -> Int) $ words m
        putStrLn $ show $ minPath (x,y))
