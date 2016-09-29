{-# LANGUAGE ExistentialQuantification #-}

--data Existential = forall a. b. => NewA a | NewB b | NewAB a b

--Show box
data ShowBox = forall a. (Show a) => ShowBox a

xs :: [ShowBox]
xs = [ShowBox 1, ShowBox "abc", ShowBox 'a']

doShow :: [ShowBox] -> String
doShow [] = ""
doShow ((ShowBox x):xs) = show x ++ doShow xs

--
class Shape_ a where
	area :: a -> Double
	perimeter :: a -> Double

data Shape = forall a. (Shape_ a) => Shape a

type Radius = Double
type Side = Double

data Circle = Circle Radius
data Rectangle = Rectangle Side Side
data Square = Square Side

instance Shape_ Circle where
	area (Circle r) = 2 * pi * r
	perimeter (Circle r) = pi * r * r

instance Shape_ Rectangle where
	area (Rectangle w h) = w * h
	perimeter (Rectangle w h) = 2*w + 2*h

instance Shape_ Square where
	area (Square s) = s * s
	perimeter (Square s) = 4 * s
	
instance Shape_ Shape where
	area (Shape s) = area s
	perimeter (Shape s) = perimeter s

circle :: Radius -> Shape
circle r = Shape $ Circle r

square :: Side -> Shape
square s = Shape $ Square s

rectangle :: Side -> Side -> Shape
rectangle w h = Shape $ Rectangle w h

shapeList :: [Shape]
shapeList = [circle 10.0, square 2.0, rectangle 2.0 4.0]


