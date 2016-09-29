module Main(main) where
    import Graphics.Gloss
    import Graphics.Gloss.Data.ViewPort
    import Graphics.Gloss.Interface.Pure.Game
    import Data
    import GameSetup
    import Renderer

    moveBall :: Float -> GameState -> GameState
    moveBall seconds gs = gs { gs_ballLocation = (x', y') }
      where
        (x, y) = gs_ballLocation gs
        (vx, vy) = gs_ballSpeed gs
        x' = x + vx * seconds
        y' = y + vy * seconds

    --Speed up detection by looking at ball travelling direction!
    recContain :: Point -> Rec -> Bool
    recContain (x, y) (wx, wy, ww, wh) = checkX && checkY
      where
        checkX = x <= wx + ww/2 && x >= wx - ww/2
        checkY = y <= wy + wh/2 && y >= wy - wh/2

    yCollision :: Point -> Rec -> Bool
    yCollision (x, y) rec = topContain || bottomContain
      where
        topContain = recContain (x, y - ballradius) rec
        bottomContain = recContain (x, y + ballradius) rec

    xCollision :: Point -> Rec -> Bool
    xCollision (x, y) rec = leftContain || rightContain
      where
        leftContain = recContain (x - ballradius, y) rec
        rightContain = recContain (x + ballradius, y) rec

    wallCollision :: GameState -> (Bool, Bool)
    wallCollision gs = (collideX, collideY)
      where
        balllocation = gs_ballLocation gs
        walls = gs_wallObjects gs ++ [gs_paddle1 gs,gs_paddle2 gs]
        collideX = any (xCollision balllocation) walls
        collideY = any (yCollision balllocation) walls

    bounce :: GameState -> GameState
    bounce gs = gs { gs_ballSpeed = (vx', vy')}
      where
        (vx, vy) = gs_ballSpeed gs
        vx' = if fst $ wallCollision gs then -vx else vx
        vy' = if snd $ wallCollision gs then -vy else vy

    movePaddle :: Movement -> Rec -> Rec -> Rec
    movePaddle m p@(px,py,pw,ph) otherPaddle = let inbound pt rec = not $ any (recContain pt) [rec,otherPaddle] in
      case m of MoveUp -> if inbound (px, py + ph/2) topWall then (px, py + paddleStep, pw, ph) else p
                MoveDown -> if inbound (px, py - ph/2) bottomWall then (px, py - paddleStep, pw, ph) else p
                MoveLeft -> if inbound (px - pw/2, py) leftWall then (px - paddleStep, py, pw, ph) else p
                MoveRight -> if inbound (px + pw/2, py) rightWall then (px + paddleStep, py, pw, ph) else p

    handleKeyPress :: Event -> GameState -> GameState
    handleKeyPress (EventKey (Char 'r') _ _ _) gs = gs { gs_ballLocation = (0,0)}
    handleKeyPress (EventKey key _ _ _) gs = newgs
      where
        newgs = handleKey key gs
    handleKeyPress _ gs = gs { gs_lastKey = (SpecialKey KeyUnknown)}

    handleKey :: Key -> GameState -> GameState
    handleKey key gs = case key of
                        (SpecialKey KeyDown) -> gs { gs_paddle1 = movePaddle MoveDown (gs_paddle1 gs) (gs_paddle2 gs) }
                        (SpecialKey KeyUp) -> gs { gs_paddle1 = movePaddle MoveUp (gs_paddle1 gs) (gs_paddle2 gs)}
                        (SpecialKey KeyLeft) -> gs { gs_paddle1 = movePaddle MoveLeft (gs_paddle1 gs) (gs_paddle2 gs)}
                        (SpecialKey KeyRight) -> gs { gs_paddle1 = movePaddle MoveRight (gs_paddle1 gs) (gs_paddle2 gs)}
                        (Char 'w') -> gs { gs_paddle2 = movePaddle MoveUp (gs_paddle2 gs) (gs_paddle1 gs) }
                        (Char 's') -> gs { gs_paddle2 = movePaddle MoveDown (gs_paddle2 gs) (gs_paddle1 gs)}
                        (Char 'a') -> gs { gs_paddle2 = movePaddle MoveLeft (gs_paddle2 gs) (gs_paddle1 gs)}
                        (Char 'd') -> gs { gs_paddle2 = movePaddle MoveRight (gs_paddle2 gs) (gs_paddle1 gs)}
                        _ -> gs { gs_lastKey = (SpecialKey KeyUnknown) }

    handleKey' :: GameState -> GameState
    handleKey' gs = handleKey (gs_lastKey gs) gs

    fps = 60::Int

    update :: Float -> GameState -> GameState
    update s = handleKey' . bounce . moveBall s

    main :: IO ()
    main = play window background fps initialState renderGame handleKeyPress update
