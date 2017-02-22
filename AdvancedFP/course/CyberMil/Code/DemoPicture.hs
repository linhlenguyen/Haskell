module DemoPicture where

import Picture
import Colour
import PPM6


r1 = Rectangle 4 3
c1 = Ellipse 8 8
t1 = RtTriangle 15 20
 
reg1 = Translate (-15,15) (Scale (5,15) (Shape r1))
reg2 = Translate (25,0) (Shape c1)
reg3 = Scale (2,3) (Shape t1)


p1 = Region green reg1 `Over` Region red reg2 `Over` Region blue reg3
gi1 = info 200 200 100
hh = vertexToPixel gi1

main = save_ppm "picture.ppm" (render p1 white gi1)


quick = quick_ppm "qpicture.ppm" (pixelColor p1 yellow gi1) 200 200