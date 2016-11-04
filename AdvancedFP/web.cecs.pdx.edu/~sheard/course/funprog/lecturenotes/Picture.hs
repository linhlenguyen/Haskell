module Picture(Picture(Region,Over,EmptyPic,Text),
               Color(Black,Blue,Green,Cyan,
                     Red,Magenta,Yellow,White),
               regionToGRegion,shapeToGRegion,
               drawRegionInWindow,drawPic,draw,spaceClose
              ) where
import Draw
import Region
import SOEGraphics hiding (Region)
import qualified SOEGraphics as G (Region)

data Picture = Region Color Region
             | Picture `Over` Picture
             | Text Vertex String
             | EmptyPic
       deriving Show


-- The Color type is imported from SOEGraphics 
-- and Exported from Picture   
-- data Color = Black | Blue | Green | Cyan
--            | Red | Magenta | Yellow | White
  
       
drawRegionInWindow::Window -> Color -> Region -> IO ()
drawRegionInWindow w c r =
   drawInWindow w 
     (withColor c (drawRegion (regionToGRegion r)))
     
drawPic :: Window -> Picture -> IO ()
drawPic w (Region c r)   = drawRegionInWindow w c r
drawPic w (p1 `Over` p2) = 
    do { drawPic w p2 ; drawPic w p1 }
drawPic w EmptyPic       = return ()
drawPic w (Text v s) = drawInWindow w (G.text (trans v) s)

 
xWin2 = xWin `div` 2 
yWin2 = yWin `div` 2 


shapeToGRegion (lx,ly) (sx,sy) s  = 
    case s of
       Rectangle s1 s2 -> createRectangle 
                            (trans (-s1/2,-s2/2)) 
                            (trans (s1/2,s2/2))
       Ellipse r1 r2 -> createEllipse 
                            (trans (-r1,-r2))
                            (trans ( r1, r2))
       Polygon pts -> createPolygon (map trans pts)
       RtTriangle s1 s2  -> createPolygon 
                             (map trans [(0,0),(s1,0),(0,s2)])
   where trans (x,y) = ( xWin2 + inchToPixel ((x+lx)*sx), 
                         yWin2 - inchToPixel ((y+ly)*sy) )
                         



regionToGRegion :: Region -> G.Region
regionToGRegion r = regToGReg (0,0) (1,1) r                          
                            
regToGReg :: Vector -> Vector -> Region -> G.Region
regToGReg (trans @ (x,y)) (sca @ (a,b)) shape =
  (case shape of
    (Shape s) -> shapeToGRegion trans sca s 
    (Translate (u,v) r) -> regToGReg (x+u, y+v) sca r
    (Scale (u,v) r) -> regToGReg trans (a*u, b*v) r
    (Empty) -> createRectangle (0,0) (0,0)
    (r1 `Union` r2) -> primGReg trans sca r1 r2 orRegion
    (r1 `Intersect` r2) -> primGReg trans sca r1 r2 andRegion
    (Complement  r) -> primGReg trans sca winRect r diffRegion
       where  winRect :: Region
              winRect = Shape (Rectangle 
                        (pixelToInch xWin) (pixelToInch yWin)))

{-
primGReg trans sca r1 r2 op
  = do gr1 <- regToGReg trans sca r1
       gr2 <- regToGReg trans sca r2
       gr3 <- op gr1 gr2
       deleteRegion gr1
       deleteRegion gr2
       return gr3
-}

primGReg trans sca r1 r2 op
  = let gr1 = regToGReg trans sca r1
        gr2 = regToGReg trans sca r2
    in op gr1 gr2
       

draw :: Picture -> IO ()
draw p
   = runGraphics (
     do w <- openWindow "Region Test" (xWin,yWin)
        drawPic w p
        spaceClose w
     )
 
