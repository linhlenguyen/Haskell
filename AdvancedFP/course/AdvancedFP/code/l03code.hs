{-# Language MultiParamTypeClasses,
     FunctionalDependencies
              #-}


-- <Pre>

class  Add arg1 arg2 res | arg1 arg2 -> res where
  plus :: arg1 -> arg2 -> res

instance  Add Int Int Int  where
  plus = primIntAdd

instance  Add Float Float Float  where
  plus = primFloatAdd

instance  Add Int Float Float  where
  plus x y = (int2Float x) + y

----------------------------------
primFloatAdd :: Float -> Float -> Float
primFloatAdd x y = x+y

primIntAdd :: Int -> Int -> Int
primIntAdd x y = x+y

int2Float :: Int -> Float
int2Float x = fromIntegral x

--------------------------------------------
-- Implicit parameters

data Term
  = Int Int
  | Plus Term Term
  | Let String Term Term
  | Var String


eval :: (String -> Int) -> Term -> M Int
eval env t = case t of
  Int n -> return n
  Var s -> return(env s)
  Plus x y -> do { a <- eval env x
                 ; b <- eval env y
                 ; return(a+b)}
  Let s x y ->  do { a <- eval env x
                   ; eval (newenv a) y }
    where newenv a z = if s==z then a else env z

-----------------------------------------------------------

instance Show Term where
  show (Int n) = show n
  show (Var x) = x
  show (Plus x y) = "("++(show x)++"+"++(show y)++")"
  show (Let s x y) = "let "++s++" = "++show x++" in "++show y


data M a = M(a,String)

instance Functor M where
  fmap f (M(x,s)) = M(f x,s)
  
instance Applicative M where
  pure x = M(x,"")
  (<*>) (M(f,s1)) (M(x,s2)) = M(y,s1++s2)
     where y = f x

instance Monad M where
  return x = M(x,"")
  (>>=) (M(a,s1)) f = M(case f a  of
                         M(b,s2) -> (b,s1++s2))

pp :: String -> M ()
pp s = M((),s)

instance Show a => Show(M a) where
  show (M(x,y)) = show x ++ "\n   "++show y

env "y" = 1
env y = error ("Not known: "++y)

ex1 = Let "x"
          (Plus (Int 3) (Var "y"))
          (Plus (Var "x") (Plus (Int 5) (Var "y")))

copies 0 s = ""
copies n s = s++(copies (n-1) s)

{-
eval2 :: (?depth :: Int) => (String -> Int) -> Term -> M Int
eval2 env t =
 do { pp (copies (?depth * 3) " ")
    ; pp ("=> "++(show t))
    ; pp "\n"
    ; ans <- (case t of
               Int n -> return n
               Var s -> return(env s)
               Plus x y -> do { a <- eval2 env x
                              ; b <- eval2 env y
                              ; return(a+b)}
               Let s x y ->  do { a <- eval2 env x
                                ; eval2 (newenv a) y }
                   where newenv a z = if s==z then a else env z)
              with ?depth = ?depth + 1
    ; pp (copies (?depth * 3) " ")
    ; pp ("<= "++(show ans))
    ; pp "\n"
    ; return ans
    }

putM :: M a -> IO ()
putM (M(_,s)) = putStrLn s

a1 = putM (eval2 env ex1 with ?depth = 0)

entering :: (?depth :: Int, Show b) => b -> M ()
entering t =
  do { pp (copies (?depth * 3) " ")
     ; pp ("=> "++(show t))
     ; pp "\n"
     }

leaving :: (?depth :: Int, Show b) => b -> M ()
leaving ans =
 do { pp (copies (?depth * 3) " ")
    ; pp ("<= "++(show ans))
    ; pp "\n"
    }

--trace :: (?depth :: Int) => (?depth::Int => Term -> M Int) -> Term -> M Int
trace f t =
 do { entering t
    ; let d = ?depth
    ; pp "AA"
    ; pp (show d)
    ; pp "ZZ"
    ; ans <- (f t) with ?depth = d + 1
    ; leaving ans
    ; return ans
    }


--eval3 :: (?depth :: Int) => ([Char] -> Int) -> Term -> M Int
eval3 env t = trace eval3f t
  where eval3f (Int n) = return n
        eval3f (Var s) = return(env s)
        eval3f (Plus x y) =
           do { a <- eval3 env x
              ; b <- eval3 env y
              ; return(a+b)}
        eval3f (Let s x y) =
           do { a <- eval3 env x
              ; eval3 (newenv a) y }
         where newenv a z = if s==z then a else env z


a2 = putM ((eval3 env ex1) with ?depth = 0)
-}

-- </Pre>
