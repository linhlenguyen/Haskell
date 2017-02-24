-- An algebraic datatype for defining shapes and
-- transformations:

type Point     = (Float, Float)
type Angle     = Float
type Radius    = Float

-- A type for describing transformations:

data Transform = Translate Point
               | Rotate Angle
               | Compose Transform Transform

-- some example transformations:

moveupByOneX   = Translate (1,0)
rotate180      = Rotate 180
combined       = Compose rotate180 moveupByOneX

-- A type for describing shapes:

data Shape     = Circle Radius
               | Polygon [Point]
               | Transform Transform Shape

-- some example shapes:

unitcircle = Circle 1
square     = Polygon [ (-1,-1), (-1,1), (1,1), (1,-1) ]
diamond    = Transform (Rotate 45) square

-- A function to determine whether a given shape is
-- circular or not.  (We're fortunate that the type
-- of transformations does not include anything that
-- distorts circles into some different shape, or
-- else the case for the Transform constructor would
-- be much harder to complete ...)

circular                :: Shape -> Bool
circular (Circle r)      = True
circular (Polygon ps)    = length ps == 1
circular (Transform t s) = circular s

