module Setup(
window,
background,
fps,
moveSpeed,
initialState
)
  where
    import qualified Graphics.Gloss as Gloss
    import Data

    window :: Gloss.Display
    window = Gloss.InWindow "Window" (720,480) (10,10)

    background :: Gloss.Color
    background = Gloss.black

    initialState :: WorldState
    initialState = WorldState {
      ws_player = Character {
        c_position = (-100, -50),
        c_action = Stop,
        c_currentSprite = Stand
      },
      ws_keyPressed = [],
      ws_background = undefined,
      ws_sprites = undefined
    }

    fps = 60::Int
    moveSpeed = 4::Float
