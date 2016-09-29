module Main where

  import Graphics.Gloss
  import Graphics.Gloss.Data.ViewPort
  --import Pong.Data
  --import Pong.Init

  data GameState = Game {
    gs_ballLocation :: Point,
    gs_ballSpeed :: Vector,
    gs_wallObjects :: [Rec],
    gs_paddle1 :: Rec,
    gs_paddle2 :: Rec
  } deriving (Show)

  type Rec = (Float, Float, Float, Float) -- (x, y, width, height)

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

  window :: Display
  window = InWindow "a Window" (720,480) (10,10)

  background :: Color
  background = white

  renderRec :: Rec -> Picture
  renderRec (x, y, w, h) = translate x y $ rectangleSolid w h

  renderRecs :: [Rec] -> Picture
  renderRecs recs = pictures $ map renderRec recs

  move :: Point -> Picture -> Picture
  move (x,y) p = translate x y p

  render :: GameState -> Picture
  render gs = pictures [
      ball, walls, p1, p2]
    where
    ball = move (gs_ballLocation gs) $ circle ballradius
    walls = renderRecs $ gs_wallObjects gs
    p1 = renderRec $ gs_paddle1 gs
    p2 = renderRec $ gs_paddle2 gs

  initialState = Game {
    gs_ballLocation = (50, 50),
    gs_ballSpeed = (-20 , 0),
    gs_paddle1 = paddle1,
    gs_paddle2 = paddle2,
    gs_wallObjects = walls
  }

  moveBall :: Float -> GameState -> GameState
  moveBall seconds gs = gs { gs_ballLocation = (x', y') }
    where
      (x, y) = gs_ballLocation gs
      (vx, vy) = gs_ballSpeed gs
      x' = x + vx * seconds
      y' = y + vy * seconds

  collide :: Point -> Rec -> Bool
  collide (x, y) (wx, wy, ww, wh) = x + ballradius >= wx - ww || x - ballradius <= wx + ww || y + ballradius >= wy - wh || y - ballradius <= wy + wh

  wallCollision :: GameState -> (Bool, Bool)
  wallCollision gs = (xCollision, yCollision)
    where
      (x, y) = gs_ballLocation gs
      ws = gs_wallObjects gs
      xCollision = null $ filter (collide (x, y)) ws
      yCollision = null $ filter (collide (x, y)) ws

  bounce :: GameState -> GameState
  bounce gs = gs { gs_ballSpeed = (vx', vy')}
    where
      (vx, vy) = gs_ballSpeed gs
      vx' = if fst $ wallCollision gs then -vx else vx
      vy' = if snd $ wallCollision gs then -vy else vy

  drawing :: Picture
  drawing = render initialState

  frame :: Float -> Picture
  frame s = render $ moveBall s initialState

  fps = 60::Int

  update :: ViewPort -> Float -> GameState -> GameState
  update _ s = bounce . moveBall s

  main :: IO ()
  main = simulate window background fps initialState render update
