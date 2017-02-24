{-# Language DeriveFunctor, FlexibleInstances  #-}


-- Some Functors

data F1 x = Zero | One | Plus x x deriving Functor
data ListF a x = Nil | Cons a x   deriving Functor
data StreamF n x = C n x          deriving Functor
data NatF x = Z | S x             deriving Functor

-----------------------------------------
-- Algebras and their duals co-Algebras

data Algebra f c = Algebra (f c -> c)

data CoAlgebra f c = CoAlgebra {unCoAlgebra:: c -> f c }

--------------------------------------------
-- Initial and Final Algebras

data Initial f = Init (f (Initial f))

data Final f = Final{ unFinal:: (f (Final f)) }

--------------------------------------------
-- Operations on Initial (cata) and Final (ana) algebras

cata :: Functor f => (Algebra f b) -> Initial f -> b
cata (Algebra phi) (Init x) = phi(fmap (cata (Algebra phi)) x)

ana :: Functor f => (CoAlgebra f seed) -> seed -> (Final f)
ana (CoAlgebra phi) seed = Final(fmap (ana (CoAlgebra phi)) (phi seed))



mapp :: (a -> b) -> Initial (ListF a) -> Initial (ListF b)
mapp f x = cata (Algebra phi) x
  where phi Nil = Init Nil
        phi (Cons x xs) = Init (Cons (f x) xs)
        
trans :: Initial (ListF a) -> [a]
trans = cata (Algebra phi)
  where phi Nil = []
        phi (Cons x xs) = x : xs
        
app = cata (Algebra phi)
  where phi Nil = id
        phi (Cons x g) = (\ ys -> Init (Cons x (g ys)))