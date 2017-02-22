-- <pre>

import Prelude hiding (catch)

-- a function, f :: a -> b -> c
-- continuation style,  f :: a -> b -> (c -> ans) -> ans

-- old (direct) style
append [] xs = xs
append (y:ys) xs = y : (append ys xs)

-- CPS style
consC :: a -> [a] -> ([a] -> ans) -> ans
consC x xs k = k(x:xs)

appendC :: [a] -> [a] -> ([a] -> ans) -> ans
appendC [] xs k = k xs
appendC (y:ys) xs k = appendC ys xs (\ zs -> consC y zs k)


---------------------------------------------------
-- another example

data Tree a = Tip a | Fork (Tree a) (Tree a)

-- direct style
flat :: Tree a -> [a]
flat (Tip x) = x : []
flat (Fork x y) = flat x ++ flat y

-- CPS style
flatC :: Tree a -> ([a] -> ans) -> ans
flatC (Tip x) k = consC x [] k
flatC (Fork x y) k = flatC y (\ zs -> flatC x (\ ws -> appendC ws zs k))

----------------------------------------------------
-- What is this good for? is it efficient?

tree1 = Fork (Fork (Tip 1) (Tip 2)) (Fork (Tip 3) (Tip 4))

double 0 x = x
double n x = double (n-1) (Fork x x)

-- Try both versions on some big trees

ex1 = length(flat (double 14 tree1))
ex2 = length(flatC (double 14 tree1) id)

{-
Main> :set +s
Main> ex1
65536
(1179828 reductions, 2359677 cells, 10 garbage collections)
Main> ex2
65536
(2425002 reductions, 5505325 cells, 34 garbage collections)

Clearly the continuation example uses more resources! why use it?
-}

-- Use continuations for explicit control of control flow
-- consider a function   prefix :: (a -> Bool) -> [a] -> Maybe[a]
-- (prefix p xs)  returns the longest prefix of xs, ys
-- such that (all p ys) && not(p (head (drop (length ys) xs)))
-- i.e. the next element does not have the property p. Return nothing
-- if all elements meet p

prefix :: (a -> Bool) -> [a] -> Maybe [a]
prefix p [] = Nothing
prefix p (x:xs) = if p x then cons x (prefix p xs) else Just []
  where cons x Nothing = Nothing
        cons x (Just xs) = Just(x:xs)

ex3 = prefix even [2,4,6,5,2,4,8]
-- Main> ex3
-- Just [2,4,6]

ex4 = prefix even [2,4,6,8,10,12,14]
-- Main> ex4
-- Nothing

-- What happens if everything in the list meets p?
-- How many calls to cons?
-- can we do better?  Use continuations!

prefixC :: (a -> Bool) -> [a] -> (Maybe [a] -> Maybe ans) -> Maybe ans
prefixC p [] k = Nothing  -- NOTE the discarded continuation!
prefixC p (x:xs) k = if p x
                     then prefixC p xs (cons x k) -- tail recursive!
                     else k (Just [])
  where cons x k (Just xs) = k (Just(x:xs))
        cons x k Nothing = error "This case is never called"

ex5 = prefixC even [2,4,6,5,2,4,8] id
ex6 = prefixC even [2,4,6,8,10,12,14] id
ex5b = prefixD even [2,4,6,5,2,4,8] Just
ex6b = prefixD even [2,4,6,8,10,12,14] Just

prefixD p [] k = Nothing
prefixD p (x:xs) k =
  if p x then prefixD p xs (\ ys -> consC x ys k) else k []
  
-- Main> ex5
-- Just [2,4,6]
-- Main> ex6
-- Nothing

prefixK :: (a -> Bool) -> [a] -> Cont (Maybe[a]) (Maybe[a])
prefixK p [] = throw Nothing
prefixK p (x:xs) =
    if p x then do { Just ys <- prefixK p xs; return(Just(x:ys)) }
           else return(Just [])

ex5K = force $ prefixK even [2,4,6,5,2,4,8]
ex6K = force $ prefixK even [2,4,6,8,10,12,14]

-- Main> ex5K
-- Just [2,4,6]
-- Main> ex6K
-- Nothing

-- Note how throw is a global abort. Its use is appropriate
-- whenever local failure, implies global failure.

--------------------------------------------------------
-- Another example - Pattern Matching

data Term = Int Int | Pair Term Term
data Pat = Pint Int | Ppair Pat Pat | Pvar String | Por Pat Pat
type Sub = Maybe[(String,Term)]

instance Show Term where
  show (Int n) = show n
  show (Pair x y) = "("++show x++","++show y++")"

(Just xs) .&. (Just ys) = Just(xs++ys)
_ .&. _ = Nothing

Nothing .|. Nothing = Nothing
(Just xs) .|. _ = Just xs
_ .|. mxs = mxs

match :: Pat -> Term -> Sub
match (Pint n) (Int m) = if n==m then Just[] else Nothing
match (Ppair p q) (Pair x y) = match p x .&. match q y
match (Pvar s) x = Just[(s,x)]
match (Por p q) x = match p x .|. match q x
match p t = Nothing

t1 = Pair (Pair (Int 5) (Int 6)) (Int 7)
p1 = Ppair (Pvar "x") (Pvar "y")
p2 = Ppair p1 (Pint 1)
p3 = Ppair p1 (Pint 7)
p4 = Por p2 p3

{-
Main> match p1 t1
Just [("x",(5,6)),("y",7)]
Main> match p2 t1
Nothing
Main> match p3 t1
Just [("x",5),("y",6)]
Main> match (Por p2 p3) t1
Just [("x",5),("y",6)]
-}

ex7 = match p4 t1
-- Main> ex7
-- Just [("x",5),("y",6)]

matchC :: Pat -> Term -> (Sub -> Maybe ans) -> Maybe ans
matchC (Pint n) (Int m) k = if n==m then k(Just[]) else Nothing
matchC (Ppair p q) (Pair x y) k =
    matchC p x (\ xs -> matchC q y (\ ys -> k(xs .&. ys)))
matchC (Pvar s) x k = k(Just[(s,x)])
matchC (Por p q) x k =
    matchC p x (\ xs -> matchC q x (\ ys -> k(xs .|. ys)))

ex8 = matchC p4 t1 id
-- Why does this return nothing?
-- Main> ex8
-- Nothing


---------
-- Here is an example with 2 continuations
-- A success continuation, and a failure continuation

matchC2 :: Pat -> Term -> (Sub -> Sub) -> (Sub -> Sub) -> Sub
matchC2 (Pint n) (Int m) good bad = if n==m then good(Just[]) else bad Nothing
matchC2 (Ppair p q) (Pair x y) good bad =
    matchC2 p x (\ xs -> matchC2 q y (\ ys -> good(xs .&. ys)) bad) bad
matchC2 (Pvar s) x good bad = good(Just[(s,x)])
matchC2 (Por p q) x good bad = matchC2 p x good (\ xs -> matchC2 q x good bad)
matchC2 _ _ good bad = bad Nothing

ex9 = matchC2 p4 t1 id id

------------------
matchK :: Pat -> Term -> (Sub -> Maybe ans) -> Maybe ans
matchK (Pint n) (Int m) k = if n==m then k(Just[]) else Nothing
matchK (Ppair p q) (Pair x y) k =
    matchK p x (\ xs -> matchK q y (\ ys -> k(xs .&. ys)))
matchK (Pvar s) x k = k(Just[(s,x)])
matchK (Por p q) x k =
    case matchK p x id of
       Nothing -> matchK q x k
       other -> k other

ex10 = matchK p4 t1 id
-- Main> ex9
-- Just [("x",5),("y",6)]

-- Note the pattern here of "catching" a possible local failure,
-- and then picking up where that left off

matchK2 :: Pat -> Term -> Cont Sub Sub
matchK2 (Pint n) (Int m) = if n==m then return(Just[]) else throw Nothing
matchK2 (Ppair p q) (Pair x y) =
  do { a <- matchK2 p x; b <- matchK2 q y; return(a .&. b) }
matchK2 (Pvar s) x = return(Just[(s,x)])
matchK2 (Por p q) x =
  do { a <- catch(matchK2 p x)
     ; case a of
         Nothing -> matchK2 q x
         other -> return other
     }

ex9K = force $ matchK2 p4 t1
-- Main> ex9K
-- Just [("x",5),("y",6)]

------------------------------------------------------
-- Interpreter in Continuation style

data Exp = Var String
         | Lam String Exp
         | App Exp Exp
         | Num Int
         | Op (Int -> Int -> Int) Exp Exp
         | Raise Exp
         | Handle Exp Exp

data V = Fun (V -> (V -> V) -> V)
       | N Int

plus,times,minus :: Exp -> Exp -> Exp
plus x y = Op (+) x y
times x y = Op (*) x y
minus x y = Op (-) x y

extend :: Eq a => (a -> b) -> b -> a -> a -> b
extend env v a b = if a==b then v else env b

eval :: (String -> V) -> Exp -> (V -> V) -> V
eval env (Var s) k = k(env s)
eval env (App x y) k =
   eval env x (\ (Fun f) -> eval env y (\ z -> f z k))
eval env (Lam s x) k =
   k(Fun (\ v k2 -> eval (extend env v s) x k2))
eval env (Num n) k = k(N n)
eval env (Op f x y) k =
   eval env x (\ (N a) -> eval env y (\ (N b) -> k (N(f a b))))


type C x = Cont U x
data U = Fun2 (U -> C U)
       | N2 Int

eval2 :: (String -> U) -> Exp -> C U
eval2 env (Var s) = return(env s)
eval2 env (App f x) =
   do { Fun2 g <- eval2 env x; y <- eval2 env x; g y }
eval2 env (Lam s x) = return(Fun2(\ v -> eval2 (extend env v s) x))
eval2 env (Op f x y) =
   do { N2 a <- eval2 env x; N2 b <- eval2 env y; return(N2(f a b)) }
eval2 env (Num n) = return(N2 n)

--------------------------------------------------

type C3 x = Cont W x
data W = Fun3 (W -> C3 W)
       | N3 Int
       | Err W

eval3 :: (String -> W) -> Exp -> C3 W
eval3 env (Var s) = return(env s)
eval3 env (App f x) =
   do { Fun3 g <- eval3 env x; y <- eval3 env x; g y }
eval3 env (Lam s x) = return(Fun3(\ v -> eval3 (extend env v s) x))
eval3 env (Op f x y) =
   do { N3 a <- eval3 env x; N3 b <- eval3 env y; return(N3(f a b)) }
eval3 env (Num n) = return(N3 n)
eval3 env (Raise e) =
   do { x <- eval3 env e; throw(Err x) }
eval3 env (Handle x y) =
   do { x <- catch (eval3 env x)
      ; case x of
         Err v -> do { Fun3 g <- eval3 env y; g v }
         v -> return v
      }




------------------------------------------------
data Cont ans x = Cont ((x -> ans) -> ans)
runCont (Cont f) = f


instance Functor (Cont ans) where
  fmap f (Cont h) = Cont g
    where g w = h (w . f)

instance Applicative (Cont ans) where
  pure x = Cont( \ f -> f x)
  (Cont f) <*> (Cont g) = error "No applicative yet for Cont"
  

instance Monad (Cont ans) where
  return x = Cont ( \ f -> f x )
  (Cont f) >>= g = Cont( \ k -> f (\ a -> runCont (g a) (\ b -> k b)) )

callCC :: ((a -> Cont ans b) -> Cont ans a) -> Cont ans a
callCC f = Cont( \ k -> runCont (f (\ a -> Cont(\ _ -> k a))) k )


force :: Cont a a -> a
force (Cont f) = f id

throw :: a -> Cont a a
throw x = Cont(\ k -> x)

catch :: Cont a a -> Cont b a
catch (Cont f) = Cont g
  where g k = k(f id)

---------------------------------------------------------------------

-- </pre>
