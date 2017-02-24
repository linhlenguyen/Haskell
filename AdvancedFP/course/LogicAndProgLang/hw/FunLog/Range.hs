module Range where

data Bnd n = MinusInf | PlusInf | Lt n | LtEQ n
  deriving Eq
data Range n = Empty | Range {lower:: Bnd n, upper:: Bnd n}
  deriving Eq

--------------------------------------------------------  
-- Operations on Ranges

minUpper:: Ord n => Bnd n -> Bnd n -> Bnd n
minUpper MinusInf x = MinusInf
minUpper x MinusInf = MinusInf
minUpper PlusInf x = x
minUpper x PlusInf = x
minUpper (Lt n) (LtEQ m) | n==m = Lt n
                         | n<m  = Lt n
                         | n>m  = LtEQ m
minUpper (LtEQ n) (Lt m) | n==m = Lt m
                         | n<m  = LtEQ n
                         | n>m  = Lt m
minUpper (LtEQ n) (LtEQ m) = LtEQ(min n m)
minUpper (Lt n) (Lt m) = Lt(min n m)

maxLower:: Ord n => Bnd n -> Bnd n -> Bnd n
maxLower MinusInf x = x
maxLower x MinusInf = x
maxLower PlusInf x = PlusInf
maxLower x PlusInf = PlusInf
maxLower (Lt n) (LtEQ m) | n==m = Lt n
                         | n<m  = LtEQ m
                         | n>m  = Lt n                         
maxLower (LtEQ n) (Lt m) | n==m = Lt n
                         | n<m  = Lt m
                         | n>m  = LtEQ n
maxLower (LtEQ n) (LtEQ m) = LtEQ(max n m)
maxLower (Lt n) (Lt m) = Lt(max n m)

intersectRange Empty x = Empty
intersectRange x Empty = Empty
intersectRange r1 r2 = 
   range (maxLower (lower r1) (lower r2))
         (minUpper (upper r1) (upper r2))


-- If the lower and upper are inconsistent then Empty

range MinusInf x = Range MinusInf x
range x PlusInf = Range PlusInf x
range (x@(Lt n))   (y@(LtEQ m)) = if n<m  then (Range x y) else Empty
range (x@(LtEQ n)) (y@(Lt m))   = if n<m  then (Range x y) else Empty
range (x@(LtEQ n)) (y@(LtEQ m)) = if n<=m then (Range x y) else Empty
range (x@(Lt n))   (y@(Lt m))   = if n<m  then (Range x y) else Empty

-- Showing Bnd and Range

showUpper PlusInf = ("< ","+Inf")
showUpper (Lt n)  = ("< ",show n)
showUpper (LtEQ n) = ("<= ",show n)

showLower MinusInf = ("-Inf ","<")
showLower (Lt n)  = (show n++" ","<")
showLower (LtEQ n) = (show n++" ","<=")

showRange Empty x = "Empty"
showRange (Range l u) x = zap (showLower l)++" "++x++" "++ zap(showUpper u)
  where zap(x,y) = x++y

instance Show n => Show (Range n) where
  show r = showRange r "x"

