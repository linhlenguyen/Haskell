
data Sort i where
  Int :: Sort Int
  Float:: Sort Float
  Bool:: Sort Bool
     
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


data Equal a b where
  Eq:: Equal a a
  
test:: Sort i -> Sort j -> Maybe(Equal i j)
test Int Int = Just Eq
test Bool Bool = Just Eq
test Float Float = Just Eq
test _ _ = Nothing
    
constants :: [Box]
constants = [Cbox Int 0, Cbox Int 1, Cbox Float 3.14159, Cbox Bool True]

variables :: [Box]
variables =  [Vbox Int "i", Vbox Float "x", Vbox Bool "test"]

unary:: [Box]
unary = [Ubox Int Int "negate" negate, Ubox Bool Bool "not" not]

binary:: [Box]
binary = [Bbox Int Int Int "+" (+)
         ,Bbox Int Int Int "*" (*)
         ]
         
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
    Just Eq -> [ Unary s1 name f e | e <- gen (n-1) i ]
    Nothing -> []
boxToExp n s1 (Bbox i j k name f) =
  case test s1 k of
    Just Eq -> [ Binary s1 name f e1 e2 | e1 <- gen (n-1) i, e2 <- gen (n-1) j ]
    Nothing -> []
           
  
  
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
 

     
-- Start Tom's modifications (

-- test previous generator:
t1 = gen 2 Int

-- auxilliary

-- bound a number between 0.0 and 1.0 by truncating if it goes out of bounds
bound :: Float -> Float
bound x | x < 0.0 = 0.0
bound x | x > 1.0 = 1.0
bound x           = x

-- Float version of (!!) (since I can't coerce between Float, Integer & Int)
flookup :: Float -> [a] -> Maybe a
flookup _ []               = Nothing
flookup x (y:ys) | x < 1.0 = Just y
flookup x (y:ys)           = flookup (x-1.0) ys

-- use Float to select an element of a list
select :: Float -> [a] -> Maybe a
select r [] = Nothing
select r xs =
  let len = length xs
      bnd = bound r -- float between 0 and 1
      idx = bnd * fromInteger (toInteger len)
  in flookup idx xs

-- for spliting stream of random numbers
split :: [a] -> ([a],[a])
split [] = ([],[])
split [x] = ([x],[])
split (x:y:zs) =
  let (xs,ys) = split zs
  in (x:xs,y:ys)


-- Sometimes we want to consider all constructors as candidates
-- But if we know the depth bound has run out, only consider terminals (otherwise generation fails)
constructors = constants ++ variables ++ unary ++ binary 
terminals    = constants ++ variables


-- Need to only examine constructors with the appropriate return type (otherwise generation fails)
filterByType :: Sort i -> [Box] -> [Box]
filterByType s [] = []
filterByType s (x:xs) =
  let rest = filterByType s xs in
  case x of
    Cbox s' _ ->
      case test s s' of
        Just Eq -> x:rest
        Nothing ->   rest
    Vbox s' _ ->
      case test s s' of
        Just Eq -> x:rest
        Nothing ->   rest
    Ubox _ s' _ _ ->
      case test s s' of
        Just Eq -> x:rest
        Nothing ->   rest
    Bbox _ _ s' _ _ ->
      case test s s' of
        Just Eq -> x:rest
        Nothing ->   rest

-- finally, generation:
gen2:: Int -> [Float] -> Sort i -> Maybe (Exp i)
gen2 n [] s          = Nothing
gen2 n rs s | n <= 0 = Nothing

gen2 1 (r:rs) s =
  do
    c <- select r
       $ filterByType s
       $ terminals
    boxToExp2 1 rs s c

gen2 n (r:rs) s =
  do
    c <- select r
       $ filterByType s
       $ constructors
    boxToExp2 n rs s c

boxToExp2 :: Int -> [Float] -> Sort i -> Box -> Maybe (Exp i)

boxToExp2 n rs s1 (Cbox s2 a) = 
  do
    -- test s1 s2
    Eq <- test s1 s2
    return (Const s1 a)

boxToExp2 n rs s1 (Vbox s2 name) = 
  do
    Eq <- test s1 s2
    return (Var s1 name)

boxToExp2 n rs s1 (Ubox i j name f) =
  do
    Eq <- test s1 j
    e <- gen2 (n-1) rs i
    return ( Unary s1 name f e )

boxToExp2 n rs s1 (Bbox i j k name f) =
  do
    Eq <- test s1 k
    -- currently:
    let (xs,ys) = split rs
    e1 <- gen2 (n-1) xs i
    e2 <- gen2 (n-1) ys j
    return ( Binary s1 name f e1 e2 {- , rs'' -} )
    -- ideally:
    -- (e1,rs') <- gen2 (n-1) rs i
    -- (e2,rs'') <- gen2 (n-1) rs' j
    -- return ( Binary s1 name f e1 e2 , rs'' )

cx :: [Float]
cx = [0.74,0.91,0.91,0.61,0.78,0.41,0.24] ++ cx
t2 n = gen2 4 (drop n cx) Int
  
-- If I redid this, I would want:
--   gen2:: Int -> [Float] -> Sort i -> Maybe (Exp i,[Float])
--   boxToExp2 :: Int -> [Float] -> Sort i -> Box -> Maybe (Exp i,[Float])
-- the stream of random numbers is under the Maybe for two reasons
--   1. it allows us to write cleaner monadic code
--   2. gen2/boxToExp2 rarely fail, if they do fail there is no harm in retrying on the tail of the stream as none of the entries of tail have been successfully used
-- Then eliminate split
  
-- End Tom's modifications )