{-# LANGUAGE BangPatterns #-}

module Random(
main
)
  where
    import System.Random
    import Data.List (foldl')

    data Coin = Head | Tail deriving (Show, Enum, Bounded)

    instance Random Coin where
      randomR (a, b) g = let (x,g') = randomR (fromEnum a, fromEnum b) g in (toEnum x, g')
      random g = randomR (minBound, maxBound) g

    main :: IO ()
    main = do
      gen <- getStdGen
      print . take 10 $ (randomRs ('a', 'z') gen)
      print . take 10 $ (randoms gen :: [Double])
      print . take 10 $ (randoms gen :: [Coin])

    count :: Int
    count = 10000

    process :: [(Double, Double)] -> (Int, Int)
    process = foldl' sumInCircle (0, 0)

    sumInCircle :: (Int, Int) -> (Double, Double) -> (Int, Int)
    sumInCircle (!ins, !total) (x, y) = (ins + if x*x + y*y < 1.0 then 1 else 0,
                                   total + 1)

    display:: (Int, Int) -> String
    display (x, y) = "pi = "  ++ (show $ 4.0 * fromIntegral x / fromIntegral y)

    prep :: [Double] -> [(Double, Double)]
    prep (a:b:r) = (a,b):prep r

    getPi = do
      g <- newStdGen
      putStrLn . display . process . take count . prep $ randoms g
