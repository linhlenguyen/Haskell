module Setup(
window,
background
)
  where
    import qualified Graphics.Gloss.Data as GlossData

    window :: GlossData.Display
    window = InWindow "Window" (720,480) (10,10)

    background :: GlossData.Color
    background = white
