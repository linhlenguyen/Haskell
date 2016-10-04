module Polygons(main)
  where
    import Setup
    import Graphics.Gloss

    aPath :: Path
    aPath = [(2,10), (50, 60), (-20, 60), (-30, -20)]

    screen :: Picture
    screen = pictures [line aPath,
                        translate 60 0 (lineLoop aPath),
                        translate (-60) (-60) (polygon aPath)]

    main :: IO ()
    main = display window background screen
