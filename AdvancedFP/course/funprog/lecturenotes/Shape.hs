module Shape (Shape(Rectangle,Ellipse,RtTriangle,Polygon),
              Radius,Side,Coordinate,Vertex,square,circle,
              distBetween,area) where

type Radius = Float
type Side = Float
type Coordinate = (Float,Float)
type Vertex = Coordinate

data Shape = Rectangle Side Side
           | Ellipse Radius Radius
           | RtTriangle Side Side
           | Polygon [ Coordinate ]
  deriving Show

circle radius = Ellipse radius radius
square side = Rectangle side side

area :: Shape -> Float

area (Rectangle s1 s2)  = s1 * s2
area (Ellipse r1 r2)    =  pi * r1 * r2
area (RtTriangle s1 s2) = (s1 *s2) / 2
area (Polygon (v1:pts)) =  polyArea pts
   where polyArea :: [ (Float,Float) ] -> Float
         polyArea (v2 : v3 : vs) = triArea v1 v2 v3 +
                                   polyArea (v3:vs)
         polyArea _ = 0
        

triArea v1 v2 v3 = 
  let a = distBetween v1 v2
      b = distBetween v2 v3
      c = distBetween v3 v1
      s = 0.5*(a+b+c)
  in sqrt (s*(s-a)*(s-b)*(s-c))
                   
                  
distBetween (x1,y1) (x2,y2) 
 = sqrt ((x1-x2)^2 + (y1-y2)^2)                   
 
 