module Resources(
loadResource
)
  where
    import qualified Graphics.Gloss.Data.Picture as Gloss
    import Data

    bmpTagMap :: [(Sprite, FilePath)]
    bmpTagMap = [
      (FaceRight, "bmp/fr.bmp"),
      (FaceLeft, "bmp/fl.bmp"),
      (MoveLeft1, "bmp/l1.bmp"),
      (MoveLeft2, "bmp/l2.bmp"),
      (MoveLeft3, "bmp/l3.bmp"),
      (MoveLeft4, "bmp/l4.bmp"),
      (MoveRight1, "bmp/r1.bmp"),
      (MoveRight2, "bmp/r2.bmp"),
      (MoveRight3, "bmp/r3.bmp"),
      (MoveRight4, "bmp/r4.bmp"),
      (Background, "bmp/bg.bmp")]

    loadResource :: IO [(Sprite, Gloss.Picture)]
    loadResource = loadResource' bmpTagMap

    loadResource' :: [(Sprite, FilePath)] -> IO [(Sprite, Gloss.Picture)]
    loadResource' [] = return []
    loadResource' ((s,p):xs) = do
      bmp <- Gloss.loadBMP p
      bmps <- loadResource' xs
      return ((s,bmp):bmps)
