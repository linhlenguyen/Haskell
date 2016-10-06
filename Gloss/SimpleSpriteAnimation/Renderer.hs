module Renderer(
renderCharacter,
renderGame
)
  where
    import Graphics.Gloss
    import Data

    renderCharacter :: Character -> Picture
    renderCharacter c = translate x y $ bitmapOfBMP bmp
      where (x,y) = c_position c
            action = c_action c
            currentSprite = c_currentSprite c
            spriteBMPMap = c_sprites c
            bmp = snd $ head $ filter (\(s,b) -> s == nextSprite action currentSprite) spriteBMPMap

    renderGame :: WorldState -> Picture
    renderGame ws = pictures [
      bitmapOfBMP $ ws_background ws,
      renderCharacter $ ws_player ws]

    nextSprite :: Action -> Sprite -> Sprite
    nextSprite MoveLeft MoveLeft1 = MoveLeft2
    nextSprite MoveLeft MoveLeft2 = MoveLeft3
    nextSprite MoveLeft MoveLeft3 = MoveLeft1
    nextSprite Stop _ = Stand
    nextSprite MoveRight MoveRight1 = MoveRight2
    nextSprite MoveRight MoveRight2 = MoveRight3
    nextSprite MoveRight MoveRight3 = MoveRight1
    nextSprite _ _ = Stand
