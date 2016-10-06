module BMP(main)
  where
    import Setup
    import Graphics.Gloss

    screen :: Picture
    screen = pictures [ circle 40 ]

    processBMP :: [Picture] -> Float -> [Picture]
    processBMP [] _ = []
    processBMP (x:xs) i = (translate (100 * i) 0 x) : processBMP xs (i+1)

    loadBMPs :: [FilePath] -> IO [Picture]
    loadBMPs [] = return []
    loadBMPs (x:xs) = do
      bmp <- loadBMP x
      bmps <- loadBMPs xs
      return (bmp:bmps)

    type ResourceTag = String
    processTag :: FilePath -> ResourceTag
    processTag [] = []
    processTag (x:xs) = case x of
                          '.' -> []
                          _ -> x : processTag xs

    loadBMPsWithTag :: [FilePath] -> IO [(FilePath, Picture)]
    loadBMPsWithTag [] = return []
    loadBMPsWithTag (x:xs) = do
      bmp <- loadBMP x
      bmps <- loadBMPsWithTag xs
      return ((x,bmp) : bmps)

    main :: IO ()
    main = do
      bmps <- loadBMPsWithTag ["test.bmp","test2.bmp","test4.bmp"]
      let processedBmps = processBMP (map (\(x,y) -> y) bmps) 0
      display window background (pictures processedBmps)

    --TODO
    --Render tiles
    --Render objects
    --Create tile map, object map
    --Sprite animation
    --
