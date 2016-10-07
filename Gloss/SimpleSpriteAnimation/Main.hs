module Main(main)
  where
    import Graphics.Gloss
    import Graphics.Gloss.Interface.Pure.Game
    import qualified Codec.BMP as Codec
    import Setup
    import Renderer
    import Data
    import Resources
    import qualified Data.Map.Strict as Map

    handleKey :: Key -> WorldState -> WorldState
    handleKey key ws =
      case key of
        (SpecialKey KeyLeft) -> ws { ws_player = player { c_action = MoveLeft,
                                                          c_position = (x - moveSpeed,y),
                                                          c_currentSprite = if currentSprite == Stand then MoveLeft1 else currentSprite } }
        (SpecialKey KeyRight) -> ws { ws_player = player { c_action = MoveRight,
                                                           c_position = (x + moveSpeed,y),
                                                           c_currentSprite = if currentSprite == Stand then MoveRight1 else currentSprite } }
        _ -> ws

      where player = ws_player ws
            currentSprite = c_currentSprite $ ws_player ws
            (x,y) = c_position $ ws_player ws

    handleKeyPress :: Event -> WorldState -> WorldState
    handleKeyPress (EventKey key Up _ _) ws = resetKey key ws
    handleKeyPress (EventKey key Down _ _) ws = ws'
      where
        keys = ws_keyPressed ws
        ws' = handleKey key $ ws { ws_keyPressed = key:keys }
    handleKeyPress _ ws = ws

    resetKey :: Key -> WorldState -> WorldState
    resetKey key ws = ws'
      where keys = ws_keyPressed ws
            player = ws_player ws
            ws' = ws { ws_keyPressed = filter (\k -> not (k == key)) keys,
                       ws_player = player {c_currentSprite = Stand} }

    keyHold :: WorldState -> WorldState
    keyHold ws = ws'
      where keys = ws_keyPressed ws
            ws' = foldr handleKey ws keys

    update :: Float -> WorldState -> WorldState
    update _ = keyHold

    main :: IO ()
    main = do
      Right bg <- Codec.readBMP "bmp/bg.bmp"
      resource <- loadResource
      let resourceMap = Map.fromList resource
      let gameState = initialState { ws_background = bg, ws_sprites = resourceMap }
      play window background fps gameState renderGame handleKeyPress update
