module CodecBMP(main)
  where
    import Codec.BMP
    import Graphics.Gloss
    import Setup
    import System.Environment

    main :: IO ()
    main = do
      [bmpfile] <- getArgs
      Right bmp <- readBMP bmpfile
      let (w,h) = bmpDimensions bmp
      let unpackedBMP = unpackBMPToRGBA32 bmp
      display window background (bitmapOfBMP bmp)
