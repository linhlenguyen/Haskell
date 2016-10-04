module Renderer(
  renderRec,
  renderRecs,
  move,
  renderGame,
) where
  import Graphics.Gloss
  import Data

  renderRec :: Rec -> Picture
  renderRec (x, y, w, h) = translate x y $ rectangleSolid w h

  renderRecs :: [Rec] -> Picture
  renderRecs recs = pictures $ map renderRec recs

  move :: Point -> Picture -> Picture
  move (x,y) p = translate x y p

  renderGame :: GameState -> Picture
  renderGame gs = pictures [
      ball, walls, p1, p2]
    where
    ball = move (gs_ballLocation gs) $ circle (gs_ballRadius gs)
    walls = renderRecs $ gs_wallObjects gs
    p1 = renderRec $ gs_paddle1 gs
    p2 = renderRec $ gs_paddle2 gs
