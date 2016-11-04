module Color where

import Data.Word

data Color = Color
    { r :: !Word8
    , g :: !Word8
    , b :: !Word8 }
    deriving (Eq, Show)

colorToList :: Color -> [Word8]
colorToList (Color r g b) = [r,g,b]

black     = Color   0   0   0
red       = Color 255   0   0
yellow    = Color 255 255   0
green     = Color   0 255   0
cyan      = Color   0 255 255
blue      = Color   0   0 255
magenta   = Color 255   0 255
white     = Color 255 255 255

maroon    = Color 128   0   0
olive     = Color 128 128   0
darkgreen = Color   0 128   0
teal      = Color   0 128 128
navy      = Color   0   0 128
purple    = Color 128   0 128
grey      = Color 128 128 128

orange    = Color 255 165   0
silver    = Color 192 192 192
