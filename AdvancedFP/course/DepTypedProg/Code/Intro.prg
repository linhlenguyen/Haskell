
x = 3


data Seq :: *0 ~> Nat ~> *0 where
  Snil :: Seq a Z
  Scons :: a -> Seq a n -> Seq a (S n)
 deriving List(s)
  
data LTE :: Nat ~> Nat ~> *0 where
  B :: LTE Z x
  N :: LTE x y -> LTE (S x) (S y)
 deriving Nat(lte)
  
proof :: LTE 5t 7t  
proof = N (N (N (N (N B))))

-- (1 3 4 T "zxc")
-- ( 1 . (3 . (4 . (T . "zxc"))))
-- (car (x . y)) ---> x
-- (cdr ( x . y)) ----> Y
-- CDR
-- CDDR
-- CDDDR
-- CD^nR
-- cd*r 3 ---> cdddr

s :: LTE 5t 2t
s = error "bad"

-- 0:: a -> a
-- 1:: (a,b) -> b
-- 2:: (a,(b,c)) -> c
-- 3:: (a,(b,(c,d))) -> d

data Proj:: *0 ~> *0 ~> *0 where
  Zero :: Proj a a
  Succ :: Proj a b -> Proj (c,a) b
 deriving Nat(d)
 
cdStar :: Proj a b -> (a -> b)
cdStar Zero x = x
cdStar (Succ n) (a,b) = cdStar n b