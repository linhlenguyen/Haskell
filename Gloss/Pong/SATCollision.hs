module SATCollision()
where

  lineLength :: Line -> Float
  lineLength ((x1,y1), (x2,y2)) = sqrt ((x1 - x2)^2 + (y1 - y2)^2)

  polygonsCollision :: Polygon -> Polygon -> Bool
  polygonsCollision (x:xs) (y:ys) = False

  circleCollision :: Circle -> Circle -> Bool
  circleCollision c1 c2 = False

  circlePolygonCollision :: Circle -> Polygon -> Bool
  circlePolygonCollision c (p:ps) = False

  --SATPolygonAndCircle :: [Line] -> (Point, Float) -> Bool
