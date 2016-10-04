module Main(main) where
    import Graphics.Gloss
    import Graphics.Gloss.Data.ViewPort
    import Graphics.Gloss.Interface.Pure.Game
    import Data
    import GameSetup
    import Renderer
    import CollisionDetection

    moveBall :: Float -> GameState -> GameState
    moveBall seconds gs = gs { gs_ballLocation = (x', y') }
      where
        (x, y) = gs_ballLocation gs
        (vx, vy) = gs_ballSpeed gs
        x' = x + vx * seconds
        y' = y + vy * seconds

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
    handleKeyPress (EventKey key Up _ _) gs = resetKey key gs
    handleKeyPress (EventKey key Down _ _) gs = newgs
      where
        keys = gs_keyPressed gs
        newgs = handleKey key ( gs { gs_keyPressed = key:keys } )
    handleKeyPress _ gs = gs

    resetKey :: Key -> GameState -> GameState
    resetKey key gs = gs'
      where keys = gs_keyPressed gs
            gs' = gs {gs_keyPressed = filter (\k -> not (k == key)) keys}

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

    handleKeyGS :: GameState -> GameState
    handleKeyGS gs = gs'
      where keys = gs_keyPressed gs
            gs' = foldr handleKey gs keys

    fps = 60::Int

    update :: Float -> GameState -> GameState
    update s = handleKeyGS . bounce . moveBall s

    main :: IO ()
    main = play window background fps initialState renderGame handleKeyPress update
