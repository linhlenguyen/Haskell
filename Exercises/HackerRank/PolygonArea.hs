-- Enter your code here. Read input from STDIN. Print output to STDOUT
{-# LANGUAGE BangPatterns #-}

import Control.Monad

triangleArea :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Double
triangleArea (x0,y0) (x1, y1) (x2, y2) = abs (0.5 * (fromIntegral (x0*(y1 - y2) + x1*(y2-y0) + x2*(y0 - y1))))

computeArea :: [(Int, Int)] -> Double
computeArea (x:xs) = foldr foldf 0 (getAreas x xs)
    where foldf !x !y = x + y
          getAreas :: (Int, Int) -> [(Int, Int)] -> [Double]
          getAreas p0 [p1,p2] = let area = (triangleArea p0 p1 p2) in area `seq` [area]
          getAreas p0 ls@(p1:p2:ps) = let area = (triangleArea p0 p1 p2) in area `seq` (area : getAreas p0 (p2:ps))

main :: IO ()
main = do
    n <- getLine
    ls <- forM [1 .. (read :: String -> Int) n] (\x -> do
        m <- getLine
        let coords = map (read :: String -> Int) $ words m
        return coords)
    let coords = map (\(x:y:xs) -> (x,y)) ls
    putStrLn $ show $ computeArea coords
