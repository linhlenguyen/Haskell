module Main where

import Shape
import Region  
import Draw hiding (spaceClose)
import Picture hiding (spaceClose)
import SOE

ex0 =
 runGraphics(
    do { w <- openWindow "First window" (300,300)
       ; drawInWindow w (text (100,200) "hello world")
       ; k <- getKey w
       ; closeWindow w
       } )

spaceClose :: Window -> IO ()
spaceClose w =
    do { k <- getKey w
       ; putStrLn [k]
       ; if k == ' ' then closeWindow w
                     else spaceClose w
       }
ex1 =
  runGraphics(
    do { w <- openWindow "Second Program" (300,300)
       ; drawInWindow w (text (100,200) "hello Again")
       ; spaceClose w
       } )


ex2 =
 runGraphics(
   do { w <- openWindow "Draw some shapes" (300,300)
      ; drawInWindow w (ellipse (0,0) (50,50))
      ; drawInWindow w 
           (shearEllipse (0,60) (100,120) (150,200))
      ; drawInWindow w
           (withColor Red (line (200,200) (299,275)))
      ; drawInWindow w 
           (polygon [(100,100),(150,100),(160,200)])
      ; drawInWindow w 
           (withColor Green 
                (polyline [(100,200),(150,200),
                           (160,299),(100,200)]))
      ; spaceClose w
      } )


fillTri x y size w =
    drawInWindow w
           (withColor Blue
            (polygon [(x,y),
                      (x+size,y),
                      (x,y-size)]))

minSize = 8

sierpinskiTri w x y size =
  if size <= minSize
     then fillTri x y size w
     else let size2 = size `div` 2
          in do { sierpinskiTri w x y size2
                ; sierpinskiTri w x (y-size2) size2
                ; sierpinskiTri w (x + size2) y size2
                }
ex3 =
  runGraphics(
    do { w <- openWindow "Sierpinski's Tri" (400,400)
       ; sierpinskiTri w 50 300 256
       ; spaceClose w
       } )

drawPoly w color points =
   drawInWindow w 
      (withColor color (polygon points))

eqTri side (x,y) =
  let xf = fromIntegral x
      yf = fromIntegral y
      sideDiv2 = side / 2.0
      height = sqrt( side*side - 
                     (sideDiv2 * sideDiv2) )
      h1third = height / 3.0
      h2third = h1third * 2.0
      f (a,b) = (round a,round b)
  in  (map f [(xf,           yf - h2third),
              (xf - sideDiv2,yf + h1third),
              (xf + sideDiv2,yf + h1third)],
       map f [(xf - sideDiv2,yf - h1third),
              (xf + sideDiv2,yf - h1third),
              (xf,yf + h2third)])

drawStar color1 color2 w side (x,y) =
   do {  let (a,b) = eqTri side (x,y)
      ; drawPoly w color1 a
      ; drawPoly w color2 b
      }
              
ex4 =
  runGraphics(
    do { w <- openWindow "Star of david"
                         (400,400)
       ; drawStar Red Green w 243 (200,200)
       ; spaceClose w
       } ) 

snow1 w color size (x,y) =
  if size <= minSize
     then return ()
     else do { drawStar color color
                     w (fromIntegral size) (x,y)
             ; sequence_ (map smaller allpoints)
             }
 where (triangle1,triangle2) =
               eqTri (fromIntegral size) (x,y)
       allpoints = (triangle1 ++ triangle2)
       smaller x = snow1 w color (size `div` 3) x

ex5 =
  runGraphics(
    do { w <- openWindow "SnowFlake 1"
                         (400,400)
       ; snow1 w Red 243 (200,200)
       ; spaceClose w
       } )  
 
snow2 w colors size (x,y) =
  if size <= minSize
     then return ()
     else do { drawPoly w (colors !! 0) triangle2
             ; drawPoly w (colors !! 1) triangle1
             ; sequence_ (map smaller allpoints)
             }
 where (triangle1,triangle2) = eqTri (fromIntegral size) (x,y)
       allpoints = (triangle1 ++ triangle2)
       smaller x = snow2 w (tail colors) (size `div` 3) x

ex6 =
  runGraphics(
    do { w <- openWindow "Snowflake" (400,400)
       ; snow2 w [Red,Blue,Green,Yellow] 243 (200,200)
       ; spaceClose w
       } )
 
ex7 =  runGraphics(
    do { w <- openWindow "Snowflake" (400,400)
       ; snow2 w (cycle [Red,Blue,Green,Yellow]) 
               243 (200,200)
       ; spaceClose w
       } )

snow3 w colors size (x,y) =
  if size <= minSize
     then return ()
     else do { drawPoly w (colors !! 0) triangle2
             ; drawPoly w (colors !! 1) triangle1
             ; snow3 w colors (size `div` 3) (x,y)
             ; sequence_ (map smaller allpoints) }
 where (triangle1,triangle2) = eqTri (fromIntegral size) (x,y)
       allpoints = (triangle1 ++ triangle2)
       smaller x = snow3 w (tail colors) (size `div` 3) x
                                   
ex8 =
  runGraphics(
    do { w <- openWindow "Snowflake" (400,400)
       ; snow3 w (cycle [Red,Blue,Green,Yellow,White]) 243 (200,200)
       ; spaceClose w } ) 

sh1,sh2,sh3,sh4 :: Shape

sh1 = Rectangle 3 2
sh2 = Ellipse 1 1.5
sh3 = RtTriangle 3 2
sh4 = Polygon [(-2.5,2.5), 
               (-1.5,2.0), 
               (-1.1,0.2),
               (-1.7,-1.0), 
               (-3.0,0)]
               
ex9
  = runGraphics (
    do w <- openWindow "Drawing Shapes" (xWin,yWin)
       drawInWindow w 
          (withColor Red  (shapeToGraphic sh1))
       drawInWindow w 
          (withColor Blue (shapeToGraphic sh2))
       spaceClose w 
    )
        
        
type ColoredShapes = [(Color,Shape)]

shs :: ColoredShapes
shs  = [(Red,sh1),(Blue,sh2),
        (Yellow,sh3),(Magenta,sh4)]
   
drawShapes :: Window -> ColoredShapes -> IO ()
drawShapes w []  = return ()
drawShapes w ((c,s):cs)
  = do drawInWindow w 
         (withColor c (shapeToGraphic s))
       drawShapes w cs

ex10
  = runGraphics (
    do w <- openWindow 
             "Drawing Shapes" (xWin,yWin)
       drawShapes w shs
       spaceClose w )
        
ex11
  = runGraphics (
     do w <- openWindow "Drawing Shapes" (xWin,yWin)
        drawShapes w (reverse coloredCircles)
        spaceClose w
    )
    
conCircles = map circle [0.2,0.4 .. 1.6]

coloredCircles = 
  zip [Black, Blue, Green, Cyan, Red, Magenta, Yellow, White]
      conCircles

r1 = Shape (Rectangle 3 2)
r2 = Shape (Ellipse 1 1.5)
r3 = Shape (RtTriangle 3 2)
r4 = Shape (Polygon [(-2.5,2.5), (-3.0,0), 
                     (-1.7,-1.0),
                     (-1.1,0.2), (-1.5,2.0)] )
                     
reg1 = r3            `Union`     --RtTriangle 
       r1            `Intersect` -- Rectangle
       Complement r2 `Union`     -- Ellispe
       r4                        -- Polygon
pic1 = Region Cyan reg1

ex12 = draw "First Region picture" pic1
                     
reg2 = let circle = Shape (Ellipse 0.5 0.5)
           square = Shape (Rectangle 1 1)
       in (Scale (2,2) circle)
          `Union` (Translate (2,1) square)
          `Union` (Translate (-2,0) square)
pic2 = Region Yellow reg2

ex13 = 
  draw "Ex 13" pic2
  
pic3 = pic2 `Over` pic1

ex14 = draw "ex14" pic3  

oneCircle   = Shape (Ellipse 1 1)
manyCircles
  = [ Translate (x,0) oneCircle | x <- [0,2..] ]
fiveCircles = foldr Union Empty (take 5 manyCircles)
pic4 = Region Magenta (Scale (0.25,0.25) fiveCircles)
ex15 = draw "Ex15" pic4



pictToList :: Picture -> [(Color,Picture.Region)]

pictToList  EmptyPic      = []
pictToList (Region c r)   = [(c,r)]
pictToList (p1 `Over` p2) 
      = pictToList p1 ++ pictToList p2


pic6 = pic4 `Over` pic2 `Over` pic1 `Over` pic3

ex16 = draw "Ex16" pic6

adjust :: [(Color,Picture.Region)] -> Vertex ->
            (Maybe (Color,Picture.Region), [(Color,Picture.Region)])

adjust []           p = (Nothing, [])
adjust ((c,r):regs) p =
     if r `containsR` p 
        then (Just (c,r), regs)
        else let (hit, rs) = adjust regs p
             in  (hit, (c,r) : rs)
             
adjust2 regs p
  = case (break (\(_,r) -> r `containsR` p) regs) of
      (top,hit:rest) -> (Just hit, top++rest)
      (_,[])         -> (Nothing, [])

             
loop :: Window -> [(Color,Picture.Region)] -> IO ()
loop w regs = 
 do clearWindow w
    sequence [ drawRegionInWindow w c r | 
                 (c,r) <- reverse regs ]
    (x,y) <- getLBP w
    case (adjust regs (pixelToInch (x - (xWin `div` 2)), 
                       pixelToInch ((yWin `div` 2) - y) )) of
       (Nothing,  _      ) -> closeWindow w
       (Just hit, newRegs) -> loop w (hit : newRegs)

draw2 :: Picture -> IO ()
draw2 pic 
  = runGraphics (
    do w <- openWindow "Picture demo" (xWin,yWin)
       loop w (pictToList pic)) 
       
p1,p2,p3,p4 :: Picture
p1 = Region Magenta r1
p2 = Region Cyan r2
p3 = Region Green r3
p4 = Region Yellow r4

pic :: Picture 
pic = foldl Over EmptyPic [p1,p2,p3,p4]
ex17 = draw2 pic
       



main = 
  do { putStrLn "Enter an integer to choose a demo."
     ; n <- readLn
     ; [ex0,ex1,ex2,ex3,ex4,ex5,ex6,ex7,ex8,ex9
       ,ex10,ex11,ex12,ex13,ex14,ex15,ex16,ex17] !! n
     }