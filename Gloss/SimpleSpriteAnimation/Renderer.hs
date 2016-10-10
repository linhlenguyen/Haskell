module Renderer(
renderCharacter,
renderGame,
nextSprite
)
  where
    import Graphics.Gloss
    import Data
    import Data.Map.Strict

    renderCharacter :: SpriteResource -> Character -> Picture
    renderCharacter sr c = translate x y $ bmp
      where (x,y) = c_position c
            action = c_action c
            currentSprite = c_currentSprite c
            bmp = sr!currentSprite

    renderGame :: SpriteResource -> WorldState -> Picture
    renderGame sr ws = pictures [
      --sr!Background,
      renderCharacter sr (ws_player ws)]

    nextSprite :: Action -> Sprite -> Sprite
    nextSprite MoveLeft MoveLeft1 = MoveLeft2
    nextSprite MoveLeft MoveLeft2 = MoveLeft3
    nextSprite MoveLeft MoveLeft3 = MoveLeft4
    nextSprite MoveLeft MoveLeft4 = MoveLeft1
    nextSprite MoveLeft _ = MoveLeft1
    nextSprite Stop MoveLeft1 = FaceLeft
    nextSprite Stop MoveLeft2 = FaceLeft
    nextSprite Stop MoveLeft3 = FaceLeft
    nextSprite Stop MoveLeft4 = FaceLeft
    nextSprite Stop MoveRight1 = FaceRight
    nextSprite Stop MoveRight2 = FaceRight
    nextSprite Stop MoveRight3 = FaceRight
    nextSprite Stop MoveRight4 = FaceRight
    nextSprite MoveRight MoveRight1 = MoveRight2
    nextSprite MoveRight MoveRight2 = MoveRight3
    nextSprite MoveRight MoveRight3 = MoveRight4
    nextSprite MoveRight MoveRight4 = MoveRight1
    nextSprite MoveRight _ = MoveRight1
    nextSprite _ _ = FaceLeft
