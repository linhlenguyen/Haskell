module Geometry(

) where
  import Graphics.Gloss

  class Shape where
    render :: Shape -> Picture
