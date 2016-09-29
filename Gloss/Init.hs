module Pong.Init(
  wallWidth,
  wallHeight,
  wallThickness,
  paddleHeight,
  paddleWidth,
  ballradius,
  walls,
  paddle1,
  paddle2
) where
  import qualified Pong.Data

  wallWidth = 270::Float
  wallHeight = 190::Float
  wallThickness = 2::Float

  paddleHeight = 45::Float
  paddleWidth = 5::Float

  ballradius = 8::Float

  walls :: [Rec]
  walls = [(0, -wallHeight/2, wallWidth, wallThickness),
           (0, wallHeight/2, wallWidth, wallThickness),
           (-wallWidth/2, 0, wallThickness, wallHeight),
           (wallWidth/2, 0, wallThickness, wallHeight)]

  paddle1 :: Rec
  paddle1 = (-100, 20, paddleWidth, paddleHeight)

  paddle2 :: Rec
  paddle2 = (100, 20, paddleWidth, paddleHeight)
