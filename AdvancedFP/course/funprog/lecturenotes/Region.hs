module Region(Region(Shape,Translate,Scale,Complement,
                     Union,Intersect,Empty),
              Vector,
              containsS,containsR,
              module Shape
              ) where
              
import Shape

-- A Region is either:
data Region = 
   Shape Shape               -- primitive shape
 | Translate Vector Region   -- translated region
 | Scale     Vector Region   -- scaled region
 | Complement Region         -- inverse of region
 | Region `Union` Region     -- union of regions
 | Region `Intersect` Region -- intersection of regions
 | Empty
       deriving Show

type Vector = (Float, Float)     
type Ray = (Vector,Vector)


isLeftOf :: Vertex -> Ray -> Bool
(px,py) `isLeftOf` ((ax,ay),(bx,by))
       = let (s,t) = (px-ax, py-ay)
             (u,v) = (px-bx, py-by)
         in  s*v >= t*u



containsS :: Shape -> Vertex -> Bool
(Rectangle s1 s2) `containsS` (x,y)
   = let t1 = s1/2
         t2 = s2/2
     in -t1<=x && x<=t1 && -t2<=y && y<=t2
(Ellipse r1 r2) `containsS` (x,y)
   = (x/r1)^2 + (y/r2)^2 <= 1
(Polygon pts) `containsS` p
   = let shiftpts = tail pts ++ [head pts]
         leftOfList = 
            map isLeftOfp(zip pts shiftpts)
         isLeftOfp p' = isLeftOf p p'
     in foldr (&&) True leftOfList
(RtTriangle s1 s2) `containsS` p
   = (Polygon [(0,0),(s1,0),(0,s2)]) `containsS` p    


   
containsR :: Region -> Vertex -> Bool
(Shape s)           `containsR` p       = 
   s `containsS` p
(Translate (u,v) r) `containsR` (x,y)   = 
   r `containsR` (x-u,y-v)
(Scale (u,v) r)     `containsR` (x,y)   = 
   r `containsR` (x/u,y/v)
(Complement r)      `containsR` p       = 
   not (r `containsR` p)
(r1 `Union` r2)     `containsR`   p     =
   r1 `containsR` p || r2 `containsR` p
(r1 `Intersect` r2) `containsR`   p     =
   r1 `containsR` p && r2 `containsR` p
Empty               `containsR`   p     = False 
  