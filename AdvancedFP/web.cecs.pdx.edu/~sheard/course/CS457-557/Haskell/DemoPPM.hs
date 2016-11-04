module PPMexamples where

import PPM6

-------------------------------------------------------------        
-- Can we plot a circle?

close :: Double -> Double -> Double -> Bool
close epsilon x y = abs(x - y) <= epsilon
        
circle1 epsilon size x y = 
   if close epsilon (x*x + y*y) size 
      then red 
      else yellow 
              
go1 = mapDouble "circlePlain.ppm" 
                (circle1 0.05 4) (-3,-3) (3,3) (420,420) 

---------------------------------------------------
-- Plotting some reference points

origin epsilon c x y 
   | close epsilon x 0 || close epsilon y 0 = green
origin _ c _ _ = c 
           
circle2 epsilon size x y = 
   if close epsilon (x*x + y*y) size 
      then red 
      else origin (epsilon/3) yellow x y   

go3 = mapDouble "circleAxis.ppm" 
                (circle2 0.05 4) (-3,-3) (3,3) (420,420) 

------------------------------------------
-- Adding some extra stuff              

circle3 epsilon size x y = 
   if close epsilon (x*x + y*y) size 
      then red 
      else if close (5*epsilon) x 2.5 && close (5*epsilon) y 2.5
              then cyan
              else origin (epsilon/3) yellow x y                 


go4 = mapDouble "circleStuff.ppm" 
                (circle3 0.05 4) (-3,-3) (3,3) (420,420) 

---------------------------------------------------------------
-- Plotting any function of x. A graphing calculator

plotFun f = mapDouble "cplot.ppm" g (-3,-3) (3,3) (420,420) 
  where g x y = if close 0.05 (f x) y 
                   then red 
                   else origin 0.02 yellow x y
  
  
g1 = let f x = x*x in plotFun f
g2 = let f x = x*x*x - 2*x*x in plotFun f


------------------------------------------------------------------
-- Plotting any list of points

plotPoints:: Double -> Colour -> Colour -> 
                      [Point] -> Double -> Double -> Colour
plotPoints epsilon background c xs x y =
  if any (near epsilon (x,y)) xs then c else background 


--------------------------------------------------------------
-- Exploring exponential growth

points :: [Point]
points = [ (double i,2.0 ** ((double i)/5.0)) | i <- [0..40] ] 


g3 = mapDouble "exponential.ppm" 
               (plotPoints 1.5 green yellow points) 
               (-1,-1) (41,260) (42*4*6,261*4)
  
---------------------------------------------------------
-- MandelBrot Fractals


next            :: Point -> Point -> Point
next (u,v) (x,y) = (x*x-y*y+u, 2*x*y+v)

mandelbrot   :: Point -> [Point]
mandelbrot p  = iterate (next p) (0,0)

fairlyClose      :: Point -> Bool
fairlyClose (u,v) = (u*u + v*v) < 100

chooseColor        :: [color] -> [Point] -> color
chooseColor palette = 
      (palette!!) . length . take n . takeWhile fairlyClose
  where n = length palette - 1

fracImage :: (Point -> [Point]) -> [Colour] -> Point -> Colour
fracImage fractal palette = chooseColor palette . fractal

ppmPalette :: [Colour]
ppmPalette  = [ color ((2*i) `mod` (ppmMax+1)) i (ppmMax-i) 
              | i<-[0..ppmMax] ]
   where ppmMax      = 31 :: Int
         color r b g = Colour (fromIntegral r / 31) 
                              (fromIntegral b / 31) 
                              (fromIntegral g / 31)


testFrac x y = fracImage mandelbrot ppmPalette (x,y)

f3 = mapDouble "TimFrac.ppm"  testFrac (-2.25, -1.5) (0.75  , 1.5)    (600,400)
f4 = mapDouble "TimFrac2.ppm" testFrac (-1.25, -0.4) (-1.04 , -0.19)  (600,400)
f5 = mapDouble "TimFrac3.ppm" testFrac (-1.17, -0.34)(-1.128, -0.298) (600,400)

inMandelbrotSet  :: Point -> Bool
inMandelbrotSet p = all fairlyClose (mandelbrot p)

approxTest    :: Int -> Point -> Bool
approxTest n p = all fairlyClose (take n (mandelbrot p))

---------------------------------------------------------------
-- A checker board pattern

xor True False = True
xor False True = True
xor _ _ = False

checkers c1 c2 x y = if evenD x `xor` evenD y then c1 else c2

evenD :: Double -> Bool
evenD x = even(floor x)

f6 = mapDouble "check.ppm" (checkers red green) 
              (0, 0) (8,8) (400,400)


------------------------------------------------------------------
-- Can we generalize to something more useful?

              -- Use a point so we have a single arg
type BitMap = (Double,Double) -> Maybe(Colour)
                                 -- Might be undefined 
                                 -- for some points
over f g args = case f args of
                  Nothing -> g args
                  Just a -> Just a
                 
draw:: String -> Colour -> BitMap -> Point -> Point -> (Int,Int) -> IO ()
draw file color bitmap ll ur dim = mapDouble file map ll ur dim
  where fromJust (Just x) = x
        fromJust Nothing = color
        map x y = (fromJust.bitmap) (x,y) 

-------------------------------------------------------
-- Some simple BitMaps

-- Every where the same color
bkgrnd:: Colour -> BitMap
bkgrnd c (x,y) = Just c

-- A checkerboard BitMap
checks c1 c2 = Just . (uncurry (checkers c1 c2)) 

-- Draw the X and Y axis
axis (x,y) = if (close 0.05 x 0) || (close 0.05 y 0)
                then Just black
                else Nothing

-- Plot any function
plot :: Colour -> (Double -> Double) -> BitMap  
plot color f (x,y) = if close 0.05 (f x) y then Just color else Nothing

-- Draw circles of radius for each point in a list
dots:: Double -> Colour -> [Point] -> BitMap
dots radius color xs (x,y) =
  if any (near radius (x,y)) xs then Just color else Nothing
  
-- Draw a square
square color (lx,ly) (ux,uy) (x,y) = 
   if x >= lx && x <= ux && y >= ly && y <= uy 
      then Just color else Nothing

f7 = draw "graph.ppm" undefined pict (-4,-4) (4,4) (400,400)
  where pict = (plot green cubic) `over` 
               (dots 0.4 red [(2,1),(-2,1)]) `over` 
               axis `over` 
               (checks white yellow)

cubic x = x*x*x - 2*x*x

---------------------------------------------------
-- Can we do more?

layer :: [BitMap] -> BitMap
layer = foldl over (const Nothing) 


-- BitMap to BitMap functions
rotate:: Double -> BitMap -> BitMap
rotate theta f = g
  where g (x,y) = f(x*c+y*s,y*c - x*s) 
          where (s,c) = (sin theta,cos theta)
          
f8 = draw "graph.ppm" undefined (rotate (pi /6) (layer ps)) (-4,-4) (4,4) (400,400)
  where ps = [plot green cubic
             , dots 0.4 red [(0,3),(-2,1)]
             ,(square cyan (-3,1) (-1,3))
             ,plot blue witch
             , axis,checks white yellow]

witch x = 1.0 / (x*x + 1)

----------------------------------------------
-- Extensions?

oval :: Point -> Point -> Double -> BitMap
oval foci1 foci2 radius = undefined

translate :: Point -> BitMap -> BitMap
translate = undefined

resize :: Double -> BitMap -> BitMap
resize scale f = undefined

-- Other shapes, transformations?
        
        

xyaxis c (x,y) = 
   if close 0.02  x 0 || close 0.05 y 0
                  then Just c else Nothing

disc c r (x,y) = if close 0.05 (x*x + y*y) r
                  then Just c else Nothing
                  
everywhere c (x,y) = Just c

check c1 c2 (x,y) = 
  if even(floor x + floor y) 
     then Just c1 
     else Just c2
rect c (lx,ly) (ux,uy) (x,y) = 
   if x >= lx && x <= ux && y >= ly && y <= uy
      then Just c else Nothing

move (deltax,deltay) f (x,y) = f (x-deltax,y-deltay)

go12 = draw "XY.ppm" blue (check black yellow) (-4,-4) (4,4) (400,400)

go13 = draw "big.ppm" blue 
       ( move (-2,-0.5) (rotate (3*pi/2) (move (2,0.5) (rect green (-3,-3) (-1,2)))) `over`
       disc blue 2 `over` 
       move (1,1) (disc red 1) `over` 
       check white yellow) 
       (-4,-4) (4,4) (400,400)

 