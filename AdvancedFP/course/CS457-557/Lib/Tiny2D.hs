module Tiny2D where

import Data.Char (ord)
import Data.Word
import Data.Maybe
import qualified Data.ByteString as B

import Color

type R     = Double
type Point = (R, R)
type Image = Point -> Maybe Color

-- overlay an image upon another image
over :: Image -> Image -> Image
over f g = \pt -> case f pt of
    Just a  -> Just a
    Nothing -> g pt

-- overlay many images
layer :: [Image] -> Image
layer = foldl over (const Nothing)

translate :: R -> R -> Image -> Image
translate h v f = \(x,y) -> f(x - h, y - v)

rotate :: R -> Image -> Image
rotate theta f = \(x,y) -> f(x*c + y*s, y*c - x*s)
    where (c, s) = (cos theta, sin theta)

background :: Color -> Image
background color = const (Just color)

plot :: Color -> (R -> R) -> Image
plot color f = \(x,y) ->
    if close 0.05 (f x) y
        then Just color
        else Nothing

-- a disk centered at (a,b)
disk :: Color -> Point -> R -> Image
disk color (a,b) radius = \(x,y) ->
    if (x-a)*(x-a) + (y-b)*(y-b) <= radius*radius
        then Just color
        else Nothing

square :: Color -> Point -> R -> Image
square color (a,b) side = \(x,y) ->
    if max ((x-a)*(x-a)) ((y-b)*(y-b)) <= side*side
        then Just color
        else Nothing

-- two-color checkerboard that spans the entire plane
checkers :: Color -> Color -> Image
checkers color1 color2 = \(x,y) ->
    if even $ floor x + floor y
        then Just color1
        else Just color2

------------------------------------------------------------

close :: R -> R -> R -> Bool
close epsilon x y = abs (x - y) <= epsilon

c2w :: Char -> Word8
c2w = fromIntegral . ord

toPPM :: Image -> Point -> Point -> Int -> Int -> [Word8] 
toPPM f (x1,y1) (x2,y2) w h = prefix ++ concatMap colorToList pixels
    where prefix   = map c2w $ "P6\n" ++ show w ++ " " ++ show h ++ " 255\n"
          size     = length prefix + 3*(w * h)
          rowDelta = (x2 - x1) / fromIntegral(w - 1)
          colDelta = (y1 - y2) / fromIntegral(h - 1)
          pixels   = [ fromMaybe black $ f(x,y)
                     | y <- take h [y2, y2 + colDelta..]
                     , x <- take w [x1, x1 + rowDelta..] ]

toFile :: String -> [Word8] -> IO ()
toFile file bitmap = B.writeFile file (B.pack bitmap)
