module Polygons(main)
  where
    import Gloss.Graphics

    window :: Display
    window = InWindow "a Window" (720,480) (10,10)

    background :: Color
    background = white

    render :: a -> Picture
    render 

    aPath :: Path
    aPath = [(2,10), (50, 60), (-20, 60), (-30, -20)]

    main :: IO ()
    main =
