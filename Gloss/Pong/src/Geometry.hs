module Geometry(
  Shape
) where
  import Graphics.Gloss

  class Shape a where
    render :: a -> Picture
    px :: a -> Float -> Float
    py :: a -> Float -> Float

  data Circle = CCircle Float

  instance Shape Circle where
    render (CCircle r) = circle r
    px (CCircle r) t = r * cos t
    py (CCircle r) t = r * sin t
