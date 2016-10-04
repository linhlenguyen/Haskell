module GameSetup(
  wallWidth, wallHeight, wallThickness,
  paddleHeight, paddleWidth,
  ballradius,
  walls,
  paddle1, paddle2,
  initialState,
  background, window,
  topWall, bottomWall,
  leftWall, rightWall,
  paddleStep
) where
  import Data
  import Graphics.Gloss
  import Graphics.Gloss.Interface.Pure.Game

  wallWidth = 470::Float
  wallHeight = 390::Float
  wallThickness = 10::Float

  bottomWall = (0, -wallHeight/2, wallWidth, wallThickness)::Rec
  topWall = (0, wallHeight/2, wallWidth, wallThickness)::Rec
  leftWall = (-wallWidth/2, 0, wallThickness, wallHeight)::Rec
  rightWall = (wallWidth/2, 0, wallThickness, wallHeight)::Rec

  walls :: [Data.Rec]
  walls = [topWall, bottomWall, leftWall, rightWall]

  paddleHeight = 45::Float
  paddleWidth = 10::Float
  ballradius = 8::Float

  paddle1 :: Data.Rec
  paddle1 = (-100, 20, paddleWidth, paddleHeight)

  paddle2 :: Data.Rec
  paddle2 = (100, 20, paddleWidth, paddleHeight)

  speed = 5::Float

  initialState = Data.Game {
    gs_ballLocation = (50, 50),
    gs_ballSpeed = (-40 * speed, -20 * speed),
    gs_paddle1 = paddle1,
    gs_paddle2 = paddle2,
    gs_wallObjects = walls,
    gs_ballRadius = ballradius,
    gs_lastKey = (SpecialKey KeyUnknown)
  }

  paddleStep = 5::Float

  window :: Display
  window = InWindow "a Window" (720,480) (10,10)

  background :: Color
  background = white
