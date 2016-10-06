module Resources(
loadResource
)
  where
    import qualified Codec.BMP as Codec
    import Data

    bmpTagMap :: [(Sprite, FilePath)]
    bmpTagMap = [
      (Stand, "bmp/fr.bmp"),
      (MoveLeft1, "bmp/l1.bmp"),
      (MoveLeft2, "bmp/l2.bmp"),
      (MoveLeft3, "bmp/l3.bmp"),
      (MoveRight1, "bmp/r1.bmp"),
      (MoveRight2, "bmp/r2.bmp"),
      (MoveRight3, "bmp/r3.bmp")]

    loadResource :: IO [(Sprite, Codec.BMP)]
    loadResource = loadResource' bmpTagMap

    loadResource' :: [(Sprite, FilePath)] -> IO [(Sprite, Codec.BMP)]
    loadResource' [] = return []
    loadResource' ((s,p):xs) = do
      Right bmp <- Codec.readBMP p
      bmps <- loadResource' xs
      return ((s,bmp):bmps)
