module Data (
  GameState (..),
  Rec,
  mtf,
  Movement (..)
)
  where
  import Graphics.Gloss
  import Graphics.Gloss.Interface.Pure.Game

  data GameState = Game {
    gs_ballLocation :: Point,
    gs_ballRadius :: Float,
    gs_ballSpeed :: Vector,
    gs_wallObjects :: [Rec],
    gs_paddle1 :: Rec,
    gs_paddle2 :: Rec,
    gs_lastKey :: Key
    } deriving (Show)

  type Rec = (Float, Float, Float, Float) -- (x, y, width, height)

  data Movement = MoveUp | MoveDown | MoveLeft | MoveRight

  mtf :: (Num a) => Movement -> (a -> a -> a)
  mtf m = case m of MoveUp -> (+)
                    MoveDown -> (-)
                    _ -> (\_ -> id)
