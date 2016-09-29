module Data (
  GameState,
  Rec
)
  where

  data GameState = Game {
    gs_ballLocation :: Point,
    gs_ballSpeed :: Vector,
    gs_wallObjects :: [Rec],
    gs_paddle1 :: Rec,
    gs_paddle2 :: Rec
    } deriving (Show)

    type Rec = (Float, Float, Float, Float) -- (x, y, width, height)
