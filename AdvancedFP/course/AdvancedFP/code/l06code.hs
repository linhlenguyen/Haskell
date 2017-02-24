
-- <PRE>
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

data Id x = Id x deriving Show

data T1 = Add1 T1 T1
        | Sub1 T1 T1
        | Mult1 T1 T1
        | Int1 Int
    deriving Show 
type Value = Int

eval1 :: T1 -> Id Value
eval1 (Add1 x y) = 
      do { x' <- eval1 x
         ; y' <- eval1 y
         ; return (x' + y')}
eval1 (Sub1 x y) = 
      do { x' <- eval1 x
         ; y' <- eval1 y
         ; return (x' - y')}
eval1 (Mult1 x y) = 
      do { x' <- eval1 x
         ; y' <- eval1 y
         ; return (x' * y')}
eval1 (Int1 n) = return n      

-------------------------------------

data Exception x = Ok x | Fail deriving Show

t2 = Add2 (Mult2 (Int2 2) (Int2 5)) (Int2 3)
t2a = Add2 (Div2 (Int2 2) (Int2 0)) (Int2 3)

data T2 = Add2 T2 T2
        | Sub2 T2 T2
        | Mult2 T2 T2
        | Int2 Int
        | Div2 T2 T2 deriving Show
        

eval2a :: T2 -> Exception Value
eval2a (Add2 x y) =
  case (eval2a x,eval2a y)of
    (Ok x', Ok y') -> Ok(x' + y')
    (_,_) -> Fail
eval2a (Sub2 x y) =
  case (eval2a x,eval2a y)of
    (Ok x', Ok y') -> Ok(x' - y')
    (_,_) -> Fail
eval2a (Mult2 x y) =
  case (eval2a x,eval2a y)of
    (Ok x', Ok y') -> Ok(x' * y')
    (_,_) -> Fail
eval2a (Int2 x) = Ok x 
eval2a (Div2 x y) =
  case (eval2a x,eval2a y)of
    (Ok x', Ok 0) -> Fail
    (Ok x', Ok y') -> Ok(div x' y')
    (_,_) -> Fail
    
eval2 :: T2 -> Exception Value
eval2 (Add2 x y) = 
 do { x' <- eval2 x
    ; y' <- eval2 y
    ; return (x' + y')}
eval2 (Sub2 x y) = 
 do { x' <- eval2 x
    ; y' <- eval2 y
    ; return (x' - y')}
eval2 (Mult2 x y) = 
 do { x' <- eval2 x
    ; y' <- eval2 y
    ; return (x' * y')}
eval2 (Int2 n) = return n 
eval2 (Div2 x y) = 
 do { x' <- eval2 x
    ; y' <- eval2 y
    ; if y'==0 
         then Fail 
         else return (div x' y')}

--------------------------------------------

data Env e x = Env (e -> x)

data T3 = Add3 T3 T3
        | Sub3 T3 T3
        | Mult3 T3 T3
        | Int3 Int
        | Let3 String T3 T3
        | Var3 String
  deriving Show        

eval3a :: T3 -> Env Map Value
eval3a (Add3 x y) =
 Env(\e -> 
     let Env f = eval3a x
         Env g = eval3a y
     in (f e) + (g e))
eval3a (Sub3 x y) =
 Env(\e -> 
     let Env f = eval3a x
         Env g = eval3a y
     in (f e) - (g e))
eval3a (Mult3 x y) =
 Env(\e -> 
     let Env f = eval3a x
         Env g = eval3a y
     in (f e) * (g e))
eval3a (Int3 n) =
 Env(\e -> n)
eval3a (Let3 s e1 e2) =
 Env(\e ->
     let Env f = eval3a e1
         env2 = (s,f e):e
         Env g = eval3a e2
     in g env2)
eval3a (Var3 s) = getEnv s
     
     
eval3 :: T3 -> Env Map Value
eval3 (Add3 x y) = 
 do { x' <- eval3 x
    ; y' <- eval3 y
    ; return (x' + y')}
eval3 (Sub3 x y) = 
 do { x' <- eval3 x
    ; y' <- eval3 y
    ; return (x' - y')}
eval3 (Mult3 x y) = 
 do { x' <- eval3 x
    ; y' <- eval3 y
    ; return (x' * y')}
eval3 (Int3 n) = return n
eval3 (Let3 s e1 e2) = 
 do { v <- eval3 e1
     ; runInNewEnv s v (eval3 e2) }
eval3 (Var3 s) = getEnv s

--------------------------------------
data Mult x = Mult [x] deriving Show

t4 = Add4 (Mult4 (Int4 2) (Int4 5)) (Int4 3)
t4a = Add4 (Mult4 (Choose4 (Int4 2) (Int4 6)) 
                  (Choose4 (Int4 5) (Int4 3)))
           (Choose4 (Int4 3) (Int4 0))
t4b = Sqrt4 (Int4 5) 
t4c = Add4 (Int4 1) (Sqrt4 (Int4 (-3)))

data T4 = Add4 T4 T4
        | Sub4 T4 T4
        | Mult4 T4 T4
        | Int4 Int
        | Choose4 T4 T4
        | Sqrt4 T4
  deriving Show        

eval4a :: T4 -> Mult Value
eval4a (Add4 x y) = 
 let Mult xs = eval4a x
     Mult ys = eval4a y
 in Mult[ x+y | x <- xs, y <- ys ]
eval4a (Sub4 x y) = 
 let Mult xs = eval4a x
     Mult ys = eval4a y
 in Mult[ x-y | x <- xs, y <- ys ]
eval4a (Mult4 x y) = 
 let Mult xs = eval4a x
     Mult ys = eval4a y
 in Mult[ x*y | x <- xs, y <- ys ]
eval4a (Int4 n) = Mult [n]
eval4a (Choose4 x y) = 
  let Mult xs = eval4a x
      Mult ys = eval4a y
  in Mult (xs++ys)
eval4a (Sqrt4 x) = 
  let Mult xs = eval4a x
  in Mult(roots xs)

roots [] = []
roots (x:xs) | x<0 = roots xs
roots (x:xs) =  y : z : roots xs
  where y = root x
        z = negate y

root:: Int -> Int
root n = floor(sqrt (fromIntegral n))

eval4 :: T4 -> Mult Value
eval4 (Add4 x y) = 
 do { x' <- eval4 x
    ; y' <- eval4 y
    ; return (x' + y')}
eval4 (Sub4 x y) = 
 do { x' <- eval4 x
    ; y' <- eval4 y
    ; return (x' - y')}
eval4 (Mult4 x y) = 
 do { x' <- eval4 x
    ; y' <- eval4 y
    ; return (x' * y')}
eval4 (Int4 n) = return n 
eval4 (Choose4 x y) = merge (eval4a x) (eval4a y)
eval4 (Sqrt4 x) = 
 do { n <- eval4 x
    ; if n < 0 
        then none 
        else merge (return (root n)) 
                   (return(negate(root n))) }
                   
merge :: Mult a -> Mult a -> Mult a
merge (Mult xs) (Mult ys) = Mult(xs++ys)
none = Mult []


--------------------------------------
data Output x = OP(x,String) deriving Show

t6 = Add6 (Mult6 (Int6 2) (Print6 "Hi" (Int6 5))) (Int6 3)
t6a = Print6 "Low" $ Add6 (Mult6 (Int6 2) (Print6 "Hi" (Int6 5))) (Int6 3)


type Message = String
data T6 = Add6 T6 T6
        | Sub6 T6 T6
        | Mult6 T6 T6
        | Int6 Int
        | Print6 Message T6
 deriving Show        

eval6a :: T6 -> Output Value
eval6a (Add6 x y) =
 let OP(x',s1) = eval6a x
     OP(y',s2) = eval6a y
 in OP(x'+y',s1++s2)
eval6a (Sub6 x y) =
 let OP(x',s1) = eval6a x
     OP(y',s2) = eval6a y
 in OP(x'-y',s1++s2)
eval6a (Mult6 x y) =
 let OP(x',s1) = eval6a x
     OP(y',s2) = eval6a y
 in OP(x'*y',s1++s2)
eval6a (Int6 n) = OP(n,"")
eval6a (Print6 mess x) =
 let OP(x',s1) = eval6a x
 in OP(x',s1++mess++(show x'))
 
eval6 :: T6 -> Output Value
eval6 (Add6 x y) = 
 do { x' <- eval6 x
    ; y' <- eval6 y
    ; return (x' + y')}
eval6 (Sub6 x y) = 
 do { x' <- eval6 x
    ; y' <- eval6 y
    ; return (x' - y')}
eval6 (Mult6 x y) = 
 do { x' <- eval6 x
    ; y' <- eval6 y
    ; return (x' * y')}
eval6 (Int6 n) = return n   
eval6 (Print6 mess x) =
 do { x' <- eval6 x
    ; printOutput (mess++(show x'))
    ; return x'}

-----------------------------------------------
---------------------------------------------------

class Monad m => Eval e v m where
  eval :: e -> m v

data Arith x 
  = Add x x
  | Sub x x 
  | Times x x
  | Int Int
 
instance (Eval e v m,Num v) 
           => Eval (Arith e) v m where
  eval (Add x y) =  
     do { x' <- eval x
        ; y' <- eval y
        ; return (x'+y') }
  eval (Sub x y) = 
     do { x' <- eval x
        ; y' <- eval y
        ; return (x'-y') }
  eval (Times x y) =
     do { x' <- eval x
        ; y' <- eval y
        ; return (x'* y') }
  eval (Int n) = return (fromIntegral n)

instance Show e => Show (Arith e) where
  show (Add x y) = (show x) ++" + "++(show y)
  show (Sub x y) = (show x) ++" - "++(show y)
  show (Times x y) = (show x) ++" * "++(show y)
  show (Int n) = (show n)
  

-------------------------------------------  
data Divisible x 
  = Div x x

class Monad m => Failure m where
  fails :: m a
  
instance (Failure m, Integral v, Eval e v m) => 
         Eval (Divisible e) v m where
  eval (Div x y) =
    do { x' <- eval x
       ; y' <- eval y
       ; if x' == 0
            then fails
            else return(x' `div` y')
       }
       
instance Show e => Show (Divisible e) where
  show (Div x y) = (show x) ++" / "++(show y)
  
------------------------------------------------

data LocalLet x
  = Let String x x
  | Var String

class Monad m => HasEnv m v where
  inNewEnv :: String -> v -> m v -> m v
  getfromEnv :: String -> m v

instance (HasEnv m v,Eval e v m) => Eval (LocalLet e) v m where
  eval (Let s x y) = 
    do { x' <- eval x
       ; inNewEnv s x' (eval y)
       }
  eval (Var s) = getfromEnv s
  
instance Show e => Show (LocalLet e) where
  show (Let s x y) = "let "++(showString s "")++" = "++(show x)++" in "++(show y)
  show (Var s) = showString s ""
  
--------------------------------------
  
data Assignment x
  = Assign String x
  | Loc String
  
class Monad m => HasStore m v where   
  getfromStore :: String -> m v
  putinStore :: String -> v -> m v
  
instance (HasStore m v,Eval e v m) => Eval (Assignment e) v m where
  eval (Assign s x) =
    do { x' <- eval x
       ; putinStore s x' }
  eval (Loc s) = getfromStore s

instance Show e => Show (Assignment e) where
  show (Assign s x) = (showString s "")++" := "++(show x)
  show (Loc s) = showString s ""
  
----------------------------------  
data Print x 
  = Write String x
  
class (Monad m,Show v) => Prints m v where
  write :: String -> v -> m v

instance (Prints m v,Eval e v m) => Eval (Print e) v m where
  eval (Write message x)  =
    do { x' <- eval x
       ; write message x' }
       
instance Show e => Show (Print e) where
  show (Write s x) = "Print "++(showString s "")++" = "++(show x) 

-------------------------------------

data Term 
  = Arith (Arith Term)
  | Divisible (Divisible Term)
  | LocalLet (LocalLet Term)
  | Assignment (Assignment Term)
  | Print (Print Term)
  
instance (Monad m, Failure m,Integral v,
          HasEnv m v, HasStore m v, Prints m v) =>
         Eval Term v m where  
  eval (Arith x) = eval x
  eval (Divisible x) = eval x
  eval (LocalLet x) = eval x
  eval (Assignment x) = eval x
  eval (Print x) = eval x


instance Show Term where
  show (Arith x) = show x
  show (Divisible x) = show x
  show (LocalLet x) = show x
  show (Assignment x) = show x
  show (Print x) = show x


type Maps x = [(String,x)]
data M v x = M(Maps v -> Maps v -> (Maybe x,String,Maps v))

instance Functor (M v) where
  fmap f (M h) = M g
    where g a b = case h a b of
                    (x,s,m) -> (fmap f x,s,m)
                    
instance Applicative (M v ) where
   pure x = M(\ st env ->(Just x,[],st))
   (<*>) x y = undefined

instance Monad (M v) where
 return x = M(\ st env -> (Just x,[],st))
 (>>=) (M f) g = M h
   where h st env = compare env (f st env)
         compare env (Nothing,op1,st1)  = (Nothing,op1,st1)
         compare env (Just x, op1,st1)  = next env op1 st1 (g x)
         next env op1 st1 (M f2)        = compare2 op1 (f2 st1 env)
         compare2 op1 (Nothing,op2,st2) = (Nothing,op1++op2,st2)
         compare2 op1 (Just y, op2,st2) = (Just y, op1++op2,st2)


instance Failure (M v) where
  fails = M(\ st env -> (Nothing,[],st))

get name ((a,b):m) = if a==name then b else get name m

instance HasEnv (M v) v where
  inNewEnv name v (M f) = M(\ st env -> f st ((name,v):env))
  getfromEnv name = M h
     where h st env = (Just(get name env),[],st)
           
instance HasStore (M v) v where
  getfromStore name = M h
     where h st env = (Just(get name st),[],st)
  putinStore name v = M h
     where h st env = (Just v,[],(name,v):st)
     
instance Show v => Prints (M v) v where
  write message v = M h
     where h st env = (Just v,message++(show v),st)


------------------------------------------
instance Functor Id where
   fmap f (Id x) = Id (f x)

instance Applicative Id where
   pure x = Id x
   (<*>) x y = undefined
   
instance Monad Id where
  return x = Id x
  (>>=) (Id x) f = f x

-----------------------------------
instance Functor Exception where
  fmap f (Ok x) = Ok(f x)
  fmap f Fail = Fail

instance Applicative Exception where
   pure x = Ok x
   (<*>) x y = undefined

instance Monad Exception where
  return x = Ok x
  (>>=) (Ok x) f = f x
  (>>=) Fail f = Fail


-----------------------------------------

instance Functor (Env e) where
  fmap f (Env h) = Env g
    where g e = f(h e)
   
instance Applicative (Env e) where
   pure x = Env (\ env -> x)
   (<*>) x y = undefined
   
instance Monad (Env e) where
  return x = Env(\ e -> x)
  (>>=) (Env f) g = Env(\ e -> let Env h = g (f e)
                               in h e)
-----------------------------------------------
instance Functor (Store s) where
  fmap f (St h) = St g
     where g s = case h s of
                  (x,s2) -> (f x,s2)

instance Applicative (Store s) where
   pure x = St(\ s -> (x,s))
   (<*>) x y = undefined

instance Monad (Store s) where
  return x = St(\ s -> (x,s))
  (>>=) (St f) g = 
      St(\ s1 -> let (x,s2) = f s1
                     (St g') = g x
                 in g' s2)
                 
type Map = [(String,Value)]   

getEnv :: String -> (Env Map Value)
getEnv nm = Env(\ s -> find s)
  where find [] = error ("Name: "++nm++" not found")
        find ((s,n):m) = if s==nm then n else find m

runInNewEnv :: String -> Int -> (Env Map Value) -> (Env Map Value)
runInNewEnv s n (Env g) = Env(\ m -> g ((s,n):m))


-------------------------    

instance Functor Mult where
  fmap f (Mult xs) = Mult(map f xs)

instance Applicative Mult where
   pure x = Mult[x]
   (<*>) x y = undefined


instance Monad Mult where
  return x = Mult[x]
  (>>=) (Mult zs) f = Mult(flat(map f zs))
     where flat [] = []
           flat ((Mult xs):zs) = xs ++ (flat zs)
 

zz x yf =
  let Mult xs = x
      ys = map yf xs
  in Mult (concat[ z | Mult z <- ys ])

---------------------------    
instance Functor Output where
  fmap f (OP(x,s)) = OP(f x,s)

instance Applicative Output where
   pure x = OP(x,"")
   (<*>) x y = undefined


instance Monad Output where
  return x = OP(x,"")
  (>>=) (OP(x,s1)) f = let OP(y,s2) = f x in OP(y,s1 ++ s2)

printOutput:: String -> Output ()
printOutput s = OP((),s)


-------------------------------------------------

data Store s x = St(s -> (x,s)) 

data T5 = Add5 T5 T5
        | Sub5 T5 T5
        | Mult5 T5 T5
        | Int5 Int
        | Var5 String
        | Assign5 String  T5
  deriving Show        
        
eval5a :: T5 -> Store Map Value
eval5a (Add5 x y) =
  St(\s-> let St f = eval5a x
              St g = eval5a y
              (x',s1) = f s
              (y',s2) = g s1
          in(x'+y',s2))
eval5a (Sub5 x y) =
  St(\s-> let St f = eval5a x
              St g = eval5a y
              (x',s1) = f s
              (y',s2) = g s1
          in(x'-y',s2))
eval5a (Mult5 x y) =
  St(\s-> let St f = eval5a x
              St g = eval5a y
              (x',s1) = f s
              (y',s2) = g s1
          in(x'*y',s2))
eval5a (Int5 n) = St(\s ->(n,s))
eval5a (Var5 s) = getStore s
eval5a (Assign5 nm x) =
 St(\s -> let St f = eval5a x
              (x',s1) = f s
              build [] = [(nm,x')]
              build ((s,v):zs) = 
               if s==nm 
                  then (s,x'):zs 
                  else (s,v):(build zs)
          in (0,build s1)) 
          
eval5 :: T5 -> Store Map Value
eval5 (Add5 x y) = 
      do {x' <- eval5 x; y' <- eval5 y; return (x' + y')}
eval5 (Sub5 x y) = 
      do {x' <- eval5 x; y' <- eval5 y; return (x' - y')}
eval5 (Mult5 x y) = 
      do {x' <- eval5 x; y' <- eval5 y; return (x' * y')}
eval5 (Int5 n) = return n  
eval5 (Var5 s) = getStore s
eval5 (Assign5 s x) = 
      do { x' <- eval5 x; putStore s x' ; return x' }


find :: Eq a => a -> [(a,b)] -> b
find nm pairs = head [ v | (n,v) <- pairs, n==nm]

update :: Eq a => a -> b -> [(a,b)] -> [(a,b)]
update nm value pairs = (nm,value) : [ (n,v) | (n,v) <- pairs, n /= nm ]

getStore :: String -> (Store Map Value)
getStore nm = St(\ s -> (find nm s,s))
 
putStore :: String -> Value -> (Store Map ())
putStore nm n = St(\ s -> ((),update nm n s))
  
      
-- </PRE>