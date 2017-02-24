import Text.Printf (printf)

step = 0.001::Double

steps :: Double -> Double -> Double -> [Double]
steps step value high = if value < high then (value + step) : steps step (value + step) high else []

genX :: [Int] -> [Int] -> Double -> Double
genX [] _ _ = 0
genX (x:xs) (y:ys) a = ((if (y < 0) then 1/(a^(-y)) else (a^y)) * (fromIntegral x)) + (genX xs ys a)

ys :: Double -> Double -> [Int] -> [Int] -> [Double]
ys a high xs ys = map (genX xs ys) (a : steps step a high)

-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b = let yValues = (ys (fromIntegral l) (fromIntegral r) a b)
                    area = foldr (+) 0 yValues
                    volume = foldr (+) 0 $ map (\x -> pi*x*x) yValues
                    in [area/1000, volume/1000]

--Input/Output.
main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines
