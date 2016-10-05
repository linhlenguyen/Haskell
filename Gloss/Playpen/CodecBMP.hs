module CodecBMP(main)
  where
    import Codec.BMP

    main :: IO ()
    main = do
      Right bmp <- readBMP "test.bmp"
      let (w,h) = bmpDimensions bmp
      let unpackedBMP = unpackBMPToRGBA32 bmp
      print w
      print h
      print unpackedBMP
