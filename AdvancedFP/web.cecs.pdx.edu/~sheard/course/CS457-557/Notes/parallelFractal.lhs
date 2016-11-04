Calculating Fractals for CS457/557, Functional Languages:

> import Control.Parallel
> import Control.Parallel.Strategies

We'll be working with points, each of which is given by a
pair of coordinates:

> type Point = (Float, Float)

The beauty and complexity of the Mandelbrot set all comes
from the following, surprisingly simple operator on Points:

> next            :: Point -> Point -> Point
> next (u,v) (x,y) = (x*x-y*y+u, 2*x*y+v)

For any point p, we can apply (next p) repeatedly, starting
at the origin, to obtain a sequence of point values:

> mandelbrot   :: Point -> [Point]
> mandelbrot p  = iterate (next p) (0,0)

A point p is a member of the Mandelbrot set if this sequence
converges.  We can capture this by looking for points for which
the generated sequence stays "fairly close" to the origin.

> fairlyClose      :: Point -> Bool
> fairlyClose (u,v) = (u*u + v*v) < 100

> inMandelbrotSet  :: Point -> Bool
> inMandelbrotSet p = all fairlyClose (mandelbrot p)

But inMandelbrotSet is not computable because it requires us
to examine infinitely many points.  Instead, we'll use a
variant that allows us to assign colors from a palette,
picking different colors depending on how quickly each
generated sequence moves away from the origin:

> fracImage        :: [color] -> Point -> color
> fracImage palette = (palette!!)
>                   . length
>                   . take n
>                   . takeWhile fairlyClose
>                   . mandelbrot
>                   where n = length palette - 1

To render a fractal image, we need to pick a grid of coordinate
points.  Grids like this can be represented by a list (one element
per row) of lists (one element per column).

> type Grid a = [[a]]

A grid of points can be constructed as follows given the number of
columns, the number of rows, and the bounds for the grid:

> grid :: Int -> Int -> Point -> Point -> Grid Point
> grid c r (xmin,ymin) (xmax,ymax)
>       = [[ (x,y) | x <- for c xmin xmax ]
>                  | y <- for r ymin ymax ]

The "for" function captures a recurring pattern that is used to
compute a sequence of evenly spaced coordinate points:

> for          :: Int -> Float -> Float -> [Float]
> for n min max = take n [min, min+delta ..]
>     where delta = (max-min) / fromIntegral (n-1)

Two specific coordinate grids that produce interesting pictures
(the first is just the traditional view of the Mandelbrot set):

> mandGrid  = grid 79 37 (-2.25, -1.5) (0.75, 1.5)

> juliaGrid = grid 79 37 (-1.5, -1.5) (1.5, 1.5)

An image maps points to colors, interpreted in some specific
setting:

> type Image color = Point -> color

We can sample an image on a grid of points to obtain a grid of
colors:

> sample :: {- NFData color => -} Grid Point -> Image color -> Grid color
> sample points image
>         = map (map image) points

Now we can combine the pieces above to draw a fractal image.
The palette of colors, the grid of coordinates, and the rendering
function that turns a grid of colors into a pic(ture) are all
parameters.  The choice of the pic type can vary from one
application to the next, depending on what kind of fractal we
want to draw:

> draw :: NFData color =>
>          [color] -> 
>           Grid Point ->
>             (Grid color -> pic) -> pic
> draw palette grid render
>       = render (sample grid (fracImage palette) `using` parList rnf)

For a first example, drawing fractals in ASCII:

> charPalette :: [Char]
> charPalette  = "    ,.`\"~:;o-!|?/<>X+={^O#%&@8*$"

> charRender  :: Grid Char -> IO ()
> charRender   = putStr . unlines
            
> example1 = draw charPalette mandGrid charRender
 
As a second example, drawing fractals as PPM files:

> type PPMcolor = (Int, Int, Int)

> ppmPalette :: [PPMcolor]
> ppmPalette  = [ (((2*i) `mod` (ppmMax+1)), i, ppmMax-i)
>               | i<-[0..ppmMax] ]
> ppmMax      = 31 :: Int

> ppmRender  :: Grid PPMcolor -> [String]
> ppmRender g = ["P3", show w ++ " " ++ show h, show ppmMax]
>               ++ [ show r ++ " " ++ show g ++ " " ++ show b
>                  | row <- g, (r,g,b) <- row ]
>               where w = length (head g)
>                     h = length g

> main = writePPM "parmand" (draw ppmPalette mandGridHi ppmRender)

> mandGridHi  = grid 1600 1200 (-2.25, -1.5) (0.75, 1.5)

> writePPM     :: String -> [String] -> IO ()
> writePPM name = writeFile (name ++ ".ppm") . unlines

Have fun!
