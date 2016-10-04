module Setup(
window,
background
)
  where
    import qualified Graphics.Gloss as Gloss

    window :: Gloss.Display
    window = Gloss.InWindow "Window" (720,480) (10,10)

    background :: Gloss.Color
    background = Gloss.white
