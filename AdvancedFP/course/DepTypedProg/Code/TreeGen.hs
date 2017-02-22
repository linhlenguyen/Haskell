
-------------------------------------------------
-- Sorts and testing sorts for equality

data Sort i where
  Int :: Sort Int
  Float:: Sort Float
  Bool:: Sort Bool
     
data Equal a b where
    Eq:: Equal a a

test:: Sort i -> Sort j -> Maybe(Equal i j)
test Int Int = Just Eq
test Bool Bool = Just Eq
test Float Float = Just Eq
test _ _ = Nothing

----------------------------------------------
-- Boxes and Expressions

-- A box is a primitive object that is used to contruct terms.
-- There are four kinds, constants, variables, unary and binary operators.

data Box
  = forall i . Cbox (Sort i) i
  | forall i . Vbox (Sort i) String
  | forall i j . Ubox (Sort i) (Sort j) String (i -> j)
  | forall i j k . Bbox (Sort i) (Sort j) (Sort k) String (i -> j -> k)
  
data Exp i where
  Const:: (Sort i) -> i -> Exp i
  Var:: (Sort i) -> String -> Exp i
  Unary:: (Sort j) -> String -> (i -> j) -> Exp i -> Exp j
  Binary:: (Sort k) -> String -> (i -> j -> k) -> Exp i -> Exp j -> Exp k

-----------------------------------------------
-- A declaritive list of the known pieces we will
-- use to construct random terms
    
constants :: [Box]
constants = [Cbox Int 0, Cbox Int 1, Cbox Float 3.14159, Cbox Bool True]

variables :: [Box]
variables =  [Vbox Int "i", Vbox Float "x", Vbox Bool "test"]

unary:: [Box]
unary = [Ubox Int Int "negate" negate
        , Ubox Bool Bool "not" not
        , Ubox Float Int "floor" floor ]

binary:: [Box]
binary = [Bbox Int Int Int "+" (+)
         ,Bbox Int Int Int "*" (*)
         ]


----------------------------------------------
-- Generating all terms of a given type.

gen:: Int -> Sort i -> [Exp i]
gen n s | n <= 0 = []
gen n s = cs ++ vs ++ us ++ bs
  where cs = concat (map (boxToExp n s) constants)
        vs = concat (map (boxToExp n s) variables)
        us = concat (map (boxToExp n s) unary)
        bs = concat (map (boxToExp n s) binary)
       
        
boxToExp :: Int -> Sort i -> Box -> [Exp i]
boxToExp n s1 (Cbox s2 a) = 
  case test s1 s2 of
    Just Eq -> [Const s1 a]
    Nothing -> []
boxToExp n s1 (Vbox s2 name) = 
  case test s1 s2 of
    Just Eq -> [Var s1 name]
    Nothing -> []  
boxToExp n s1 (Ubox i j name f) =
  case test s1 j of
    Just Eq -> [ Unary s1 name f e 
               | e <- gen (n-1) i ]
    Nothing -> []
boxToExp n s1 (Bbox i j k name f) =
  case test s1 k of
    Just Eq -> [ Binary s1 name f e1 e2 
               | e1 <- gen (n-1) i
               , e2 <- gen (n-1) j ]
    Nothing -> []

e1 = (gen 3 Int) !! 1000
----------------------------------------
-- evaluation

data Binding = forall i . Bind (Sort i) String i

find:: Sort i -> String -> [Binding] -> Maybe i
find t1 nam [] = Nothing
find t1 name (Bind t2 n i : bs) | name==n =
  case test t1 t2 of
    Just Eq -> Just i
    Nothing -> find t1 name bs
find t1 name (b : bs) = find t1 name bs    

eval:: [Binding] -> Exp i -> i
eval env (Const ty i) = i
eval env (Unary ty name f x) = f (eval env x)
eval env (Binary ty name f x y) = f (eval env x) (eval env y)
eval env (Var t1 name) =
  case find t1 name env of
    Just i -> i
    Nothing -> error ("Unknown variable "++name)

-- Now, given a certain domain or problem, we will want to
- create a bunch of real Haskell functions that can be
-- tested for fitness. All these functions will be the same
-- type. A problem is a way of specifying the type of
-- these particular functions.
    
data Problem i where
  Pnil:: Exp i -> Problem i 
  Pcons:: Sort i -> String -> Problem j -> Problem  (i -> j)

-- The function new creates problems with given domains.

new :: Exp j -> Problem (Int -> Float -> Bool -> j)
new exp = Pcons Int "i" (Pcons Float "x" (Pcons Bool "test" (Pnil exp)))

-- And finally a problem can be converted into a real Haskell function.

run:: Problem a -> a
run p = help [] p
  where help:: [Binding] -> Problem a -> a
        help env (Pnil exp) = eval env exp
        help env (Pcons ty nam tail) =
          \ x -> help (Bind ty nam x : env) tail
           

  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ----------------------------------------
instance Show (Sort i) where
    show Int = "Int"
    show Float = "Float"
    show Bool = "Bool"
  
showS:: Sort i -> i ->String
showS Int n = show n
showS Float n = show n
showS Bool n = show n
  
instance Show Box where
    show (Cbox s i) = showS s i 
    show (Vbox s v) = v
    show (Ubox i j s f) = s
    show (Bbox i j k s f) = s

instance Show (Exp i) where
    show (Const s i) = showS s i 
    show (Var s v) = v
    show (Unary i j s f) = j++ " " ++ show f
    show (Binary i j k s f) = "("++show s ++" "++j++" "++ show f++")"
      