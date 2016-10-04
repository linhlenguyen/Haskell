--Super basic collision detection implementation
module CollisionDetection(
recContain,
yCollision,
xCollision
)
where
  import Graphics.Gloss
  import Data
  import GameSetup
  --Speed up detection by looking at ball travelling direction!
  recContain :: Point -> Rec -> Bool
  recContain (x, y) (wx, wy, ww, wh) = checkX && checkY
    where
      checkX = x <= wx + ww/2 && x >= wx - ww/2
      checkY = y <= wy + wh/2 && y >= wy - wh/2

  yCollision :: Point -> Rec -> Bool
  yCollision p@(x, y) rec = topContain || bottomContain
    where
      topContain = recContain (x, y - ballradius) rec
      bottomContain = recContain (x, y + ballradius) rec

  xCollision :: Point -> Rec -> Bool
  xCollision p@(x, y) rec = leftContain || rightContain
    where
      leftContain = recContain (x - ballradius, y) rec
      rightContain = recContain (x + ballradius, y) rec
