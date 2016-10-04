module BMP(main)
  where
    import Setup
    import Graphics.Gloss

    screen :: Picture
    screen = pictures [ circle 40 ]

    processBMP :: [Picture] -> Float -> [Picture]
    processBMP [] _ = []
    processBMP (x:xs) i = (translate (500 * i) 0 x) : processBMP xs (i+1)

    loadBMPs :: [FilePath] -> IO [Picture]
    loadBMPs [] = return []
    loadBMPs (x:xs) = do
      bmp <- loadBMP x
      bmps <- loadBMPs xs
      return (bmp:bmps)

    main :: IO ()
    main = do
      bmps <- loadBMPs ["test.bmp","test2.bmp"]
      processedBmps <- return $ processBMP bmps 0
      display window background (pictures processedBmps)
