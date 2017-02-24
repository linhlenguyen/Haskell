import Random


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

sortOf:: Exp i -> Sort i
sortOf (Const s n) = s 
sortOf (Var s name) = s
sortOf (Unary s _ _ _) = s
sortOf (Binary s _ _ _ _) = s

compatible:: (Sort i, Sort j, Sort k) -> Box -> Bool
compatible (si,sj,sk) (Bbox a b c name f) = case (test si a, test sj b, test sk c) of 
                                              (Just Eq, Just Eq,Just Eq) -> True
                                              _ -> False
compatible _ _ = False 

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
           

data MutateOp = R|RN|TL|TR
 

mutation:: [Float] ->  Exp i -> (Exp i, [Float])
mutation f (Const s1 a) = (Const s1 a, f)       -- no mutation if the Exp is only a Const and Var
mutation f (Var s name) = (Var s name, f)
mutation f exp = mutate f exp exp
                   

mutate::[Float] -> Exp b -> Exp i -> (Exp i, [Float])
mutate x exp (Const s1 a) =  case find x exp s1 of 
                               Just rep -> (rep,x)
                               Nothing -> (Const s1 a,x)
mutate x exp (Var s1 var) = case find x exp s1 of 
                               Just rep -> (rep,x)
                               Nothing -> (Var s1 var,x)
mutate (x:xs) exp (Unary s name f expl) = case nextMove x of 
                                           R -> case find xs exp s of 
                                                 Just rep -> (rep,xs)
                                                 Nothing -> (Unary s name f expl,xs) 
                                           _ -> (Unary s name f (fst subMutate), snd subMutate)
                                                   where subMutate = mutate xs exp expl
mutate (x:xs) exp (Binary s name f expl expr) = case nextMove x of 
                                                  R -> case find xs exp s of 
                                                        Just rep -> (rep,xs)
                                                        Nothing -> (Binary s name f expl expr,xs) 
                                                  TL -> (Binary s name f (fst subMutate) expr, snd subMutate)
                                                          where subMutate = mutate xs exp expl
                                                  TR -> (Binary s name f expl (fst subMutate), snd subMutate)
                                                          where subMutate = mutate xs exp expr
                                                  RN -> nextOp xs expl expr s 

nextOp:: [Float] -> Exp i -> Exp j -> Sort k -> (Exp k,[Float]) 
nextOp (x:xs) e1 e2 s = case pickOne x opEquals of 
                          (Bbox i j k name f) -> 
                                     case (test (sortOf e1) i, test (sortOf e2) j , test s k) of 
                                      (Just Eq, Just Eq, Just Eq) -> (Binary s name f e1 e2,xs) 
                                      _ -> error "not possible"
                          
  where opEquals = filter (compatible (sortOf e1,sortOf e2,s)) binary 
       
 --Bbox (Sort i) (Sort j) (Sort k) String (i -> j -> k)

pickOne:: Float -> [a] -> a
pickOne f xs = xs!!m 
 where n = length xs 
       m  = mod (floor (fromIntegral n * f)) n
 
find::[Float] -> Exp i -> Sort a -> Maybe (Exp a)
find f (Const s val) s1 = case (test s s1) of 
                            (Just Eq) -> Just (Const s val)
                            Nothing -> Nothing
find f (Var s var) s1 = case (test s s1) of 
                            (Just Eq) -> Just (Var s var)
                            Nothing -> Nothing
find (x:xs) (Unary s name f exp) s1 =case (test s s1) of 
                                     Just Eq ->  case (nextMove x) of 
                                                   R -> Just (Unary s name f exp)
                                                   RN -> Just (Unary s name f exp)
                                                   _ -> find xs exp s1
                                     Nothing -> find xs exp s1  
find (x:xs) (Binary s name f expl expr) s1 = case (test s s1) of 
                                              Just Eq ->  case (nextMove x) of 
                                                           R -> Just (Binary s name f expl expr)
                                                           RN -> Just (Binary s name f expl expr)
                                                           TL -> find xs expl s1
                                                           TR -> find xs expr s1
                                              Nothing ->  case (nextMove x) of 
                                                           TL -> find xs expl s1
                                                           TR -> find xs expr s1
                                                           _ -> Nothing

rands::[Float]
rands = randomList 9 :: [Float]

randomList::(Random a) => Int -> [a]
randomList seed = randoms (mkStdGen seed)

nextMove:: Float -> MutateOp 
nextMove f | 0 < f && f < 0.44 = TL              -- left traversal  
           | 0.45 <= f && f < 0.5 = R            -- replace sub tree
           | 0.5 <= f && f < 0.55 = RN           -- replace node      
           | 0.55 <= f && f < 1 = TR             -- right traversal 

rand::[Float]
rand = [0.52,0.15,0.05,0.23,0.33,0.35,0.21,0.63,0.22,0.41,0.2,0.45,0.83,0.93,0.99,0.73,0.64]

exp1 = Binary Int "+" (+) (Var Int "a") (Const Int 5)

exp2 = (Binary Int "+" (+) (Binary Int "*" (*) (Const Int 5) (Var Int "a")) 
                        (Binary Int "+" (+) (Var Int "b") 
                                          (Unary Int "negate" negate (Const Int 4))))  


  
  
  
  
  
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
      
