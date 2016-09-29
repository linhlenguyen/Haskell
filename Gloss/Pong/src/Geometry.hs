module Geometry(
  Shape
) where
  import qualified Graphics.Gloss

  class Shape a where
    render :: a -> Picture
    px :: a -> Float -> Float
    py :: a -> Float -> Float

  data Circle = g_circle Float

  instance Shape Circle where
    render (g_circle r) = circle r
    px (g_circle r) t = r * cos t
    py (g_circle r) t = r * sin t
