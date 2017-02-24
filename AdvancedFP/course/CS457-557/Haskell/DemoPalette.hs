module DemoPalette where


import Colour
import PPM6


redPalette x y = Colour 0.5 (fromIntegral (x - 1) / 255) (fromIntegral(y - 1) / 255)
bluePalette x y = Colour (fromIntegral (x - 1) / 255) 0.5 (fromIntegral(y - 1) / 255)
greenPalette x y = Colour (fromIntegral (x - 1) / 255) (fromIntegral(y - 1) / 255) 0.5 


main = quick_ppm "redPalette.ppm" redPalette 256 256

bp = mapPixel "bluePalette.ppm" bluePalette 256 256
gp = mapPixel "greenPalette.ppm" greenPalette 256 256
