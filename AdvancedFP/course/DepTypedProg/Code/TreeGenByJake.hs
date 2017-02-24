data Sort i where
  Int :: Sort Int
  Float:: Sort Float
  Bool:: Sort Bool

data ExSort where
  Pack :: Sort i -> ExSort

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
  Eq :: Equal a a
  
test :: Sort i -> Sort j -> Maybe(Equal i j)
test Int Int = Just Eq
test Bool Bool = Just Eq
test Float Float = Just Eq
test _ _ = Nothing
    
constants :: [Box]
constants = [Cbox Int 0, Cbox Int 1, Cbox Float 3.14159, Cbox Bool True]

variables :: [Box]
variables =  [Vbox Int "i", Vbox Float "x", Vbox Bool "test"]

unary :: [Box]
unary = [Ubox Int Int "negate" negate, Ubox Bool Bool "not" not]

binary :: [Box]
binary = [Bbox Int Int Int "+" (+)
         ,Bbox Int Int Int "*" (*)
         ]
         
gen :: Int -> Sort i -> [Exp i]
gen n s | n <= 0 = []
gen n s = cs ++ vs ++ us ++ bs
  where cs = concatMap (boxToExp n s) constants
        vs = concatMap (boxToExp n s) variables
        us = concatMap (boxToExp n s) unary
        bs = concatMap (boxToExp n s) binary

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

oneOf :: [Float] -> [x] -> (Maybe x, [Float])
oneOf [] xs = (Nothing, [])
oneOf fs [] = (Nothing, fs)
oneOf (f:fs) xs = (Just (xs!!index), fs)
  where flength = (fromIntegral . length)
        index = floor ((flength xs) * f)

data Path i where
  S :: Sort i -> Path i
  C :: Sort i -> Path j -> Path i
  L :: Sort i -> Path j -> Path i
  R :: Sort i -> Path j -> Path i

expToPaths :: Exp i -> [Path i]
expToPaths (Const s i) = [S s]
expToPaths (Var s name) = [S s]
expToPaths (Unary s name f i) = (map (C s) (expToPaths i)) ++ [S s]
expToPaths (Binary s name f i j) = (map (L s) (expToPaths i)) ++ (map (R s) (expToPaths j)) ++ [S s]

expToPathsOfSort :: forall i j . Sort i -> Exp j -> [Path j]
expToPathsOfSort st (Const s i) = hs
  where hs :: [Path j]
        hs = case test st s of
              Just Eq -> [S s]
              Nothing -> []
expToPathsOfSort st (Var s name) = hs
  where hs :: [Path j]
        hs = case test st s of
              Just Eq -> [S s]
              Nothing -> []
expToPathsOfSort st (Unary s name f i) = (map (C s) (expToPathsOfSort st i)) ++ hs
  where hs :: [Path j]
        hs = case test st s of
              Just Eq -> [S s]
              Nothing -> []
expToPathsOfSort st (Binary s name f i j) = (map (L s) (expToPathsOfSort st i)) ++ (map (R s) (expToPathsOfSort st j)) ++ hs
  where hs :: [Path j]
        hs = case test st s of
              Just Eq -> [S s]
              Nothing -> []

pathToSort :: Path i -> ExSort
pathToSort (S s) = Pack s
pathToSort (C s p) = pathToSort p
pathToSort (L s p) = pathToSort p
pathToSort (R s p) = pathToSort p

eS :: Exp i -> Sort i
eS (Binary k name f i j) = k
eS (Unary j name f i) = j
eS (Const s i) = s
eS (Var s name) = s

testPE :: Path i -> Exp j -> Maybe (Equal i j)
testPE (L s1 p) e = test s1 (eS e)
testPE (R s1 p) e = test s1 (eS e)
testPE (C s1 p) e = test s1 (eS e)
testPE (S s1) e = test s1 (eS e)

testEE :: Exp i -> Exp j -> Maybe (Equal i j)
testEE e1 e2 = test (eS e1) (eS e2)

pushIn :: Exp i -> Path j -> Exp j -> Maybe (Exp j)
pushIn e1 (L s p) (Binary k name f i j) =
  do Eq <- testEE e1 i
     return (Binary k name f e1 j)
pushIn e1 (R s p) (Binary k name f i j) =
  do Eq <- testEE e1 j
     return (Binary k name f i e1)
pushIn e1 (C s p) (Unary j name f i) =
  do Eq <- testEE e1 i
     return (Unary j name f e1)
pushIn e1 (S s) e2 =
  do Eq <- testEE e1 e2
     return e1

swapNodes :: Path i -> Exp i -> Path j -> Exp j -> Maybe (Exp i, Exp j)
swapNodes (L s1 p) (Binary k name f i j) p2 e2 =
  do Eq <- testPE p i
     (r1,r2) <- swapNodes p i p2 e2
     rS <- pushIn r2 p2 e2
     return (Binary k name f r1 j, rS)
swapNodes (R s1 p) (Binary k name f i j) p2 e2 =
  do Eq <- testPE p j
     (r1,r2) <- swapNodes p j p2 e2
     rS <- pushIn r2 p2 e2
     return (Binary k name f i r1, rS)
swapNodes (C s1 p) (Unary j name f i) p2 e2 =
  do Eq <- testPE p i
     (r1,r2) <- swapNodes p i p2 e2
     rS <- pushIn r2 p2 e2
     return (Unary j name f r1, rS)
swapNodes (S s1) e1 (L s2 p) (Binary k name f i j) =
  do Eq <- testPE p i
     (r1, r2) <- swapNodes (S s1) e1 p i
     return (r1, Binary k name f r2 j)
swapNodes (S s1) e1 (R s2 p) (Binary k name f i j) =
  do Eq <- testPE p j
     (r1, r2) <- swapNodes (S s1) e1 p j
     return (r1, Binary k name f i r2)
swapNodes (S s1) e1 (C s2 p) (Unary j name f i) =
  do Eq <- testPE p i
     (r1, r2) <- swapNodes (S s1) e1 p i
     return (r1, Unary j name f r2)
swapNodes (S s1) e1 (S s2) e2 =
  do Eq <- test s1 s2
     return (e2, e1)
swapNodes _ e1 _ e2 = Nothing

crossover :: [Float] -> Exp i -> Exp j -> (Maybe (Exp i, Exp j), [Float])
crossover fs e1 e2 = (m, rfs)
  where o1 = oneOf fs (expToPaths e1)
        fs2 = snd o1
        oM = do p1 <- fst o1
                case pathToSort p1 of
                  Pack s -> return (oneOf fs2 (expToPathsOfSort s e2))
        m = do p1 <- fst o1
               o2 <- oM
               p2 <- fst o2
               swapNodes p1 e1 p2 e2
        rfs = case oM of
                Just o2 -> snd o2
                Nothing -> fs2

--data Exp i where
--  Const:: (Sort i) -> i -> Exp i
--  Var:: (Sort i) -> String -> Exp i
--  Unary:: (Sort j) -> String -> (i -> j) -> Exp i -> Exp j
--  Binary:: (Sort k) -> String -> (i -> j -> k) -> Exp i -> Exp j -> Exp k
  
----------------------------------------
instance Show i => Show (Sort i) where
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

instance Show (Path i) where
  show (S s)   = "S " ++ show s
  show (C s p) = "C (" ++ show p ++ ")"
  show (L s p) = "L (" ++ show p ++ ")"
  show (R s p) = "R (" ++ show p ++ ")"

instance Show Height where
  show h = show (heightToInt h)

