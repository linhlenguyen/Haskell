module Sat where

import SimpleProp
import Data.List((\\))

import qualified Control.Exception


----------------------------------------------------------
-- This is the same function we used for implementing CNF
                  
data Discrim a = Alpha a a | Beta a a | Lit a
 deriving Show

discrim :: Prop a -> Discrim (Prop a)
discrim TruthP = Lit TruthP
discrim AbsurdP = Lit AbsurdP
discrim (LetterP s) = Lit (LetterP s)
discrim (AndP x y) = Alpha x y
discrim (OrP x y) = Beta x y
discrim (ImpliesP x y) = Beta (NotP x) y
discrim (NotP (OrP x y)) = Alpha (NotP x)  (NotP y)
discrim (NotP (ImpliesP x y)) = Alpha x (NotP y)
discrim (NotP (AndP x y)) = Beta (NotP x) (NotP y)
discrim (NotP (NotP x)) = discrim x
discrim (NotP TruthP) = Lit AbsurdP
discrim (NotP AbsurdP) = Lit TruthP
discrim (NotP (LetterP s)) = Lit (NotP (LetterP s))

-----------------------------------------------------------

process [] assignments = assignments
process (x:xs) assignments =
  case discrim x of
    Alpha a b -> process (a:(b:xs)) assignments
    Beta a b -> process (a:xs) assignments ++ process (b:xs) assignments
    Lit p -> process xs (foldr (extend p) [] assignments)

extend AbsurdP xs xss = xs : xss
extend TruthP xs xss = xss
extend x xs xss
  | elem x xs = xs : xss
  | elem (conj x) xs = xss
  | otherwise = (x : xs) : xss

conj TruthP = AbsurdP
conj AbsurdP = TruthP
conj (NotP x) = x
conj x = NotP x

sat x = process [x] [[]]    
    
-------------------------------------    
a = LetterP "a"
b = LetterP "b"
c = LetterP "c"
d = LetterP "d"
n x = NotP x

andL = foldr and TruthP
  where and TruthP x = x
        and x TruthP = x
        and x y = AndP x y
orL = foldr or AbsurdP
  where or AbsurdP x = x
        or x AbsurdP = x
        or x y = OrP x y

t1 = andL [orL [n a,b], orL[n a,c], orL[n b,n c,d] ]  

isTruth TruthP = True
isTruth x = False

isAbsurd AbsurdP = True
isAbsurd x = False

-- Evaluate a literal under an assignment
eval assignment TruthP = TruthP
eval assignment AbsurdP = AbsurdP
eval assignment (LetterP x) =
  case lookup x assignment of
    Just t -> t
    Nothing -> (LetterP x)
eval assignment (NotP x) = conj(eval assignment x)

-- Add a Literal to an assignment that makes the literal True
add (LetterP x) xs = (x,TruthP):xs
add (NotP (LetterP x)) xs = (x,AbsurdP):xs

-- propagate unit clauses
-- prop:: Eq n => ([[Prop n]],[(n,Prop n)]) -> Maybe ([[Prop n]],[(n,Prop n)])
prop ([],assignment) = return([],assignment)
prop (cl : cls,assignment) = 
  do { let evalClause = [ v | t <- cl, v <- [eval assignment t], not(v == AbsurdP)]
     ; putStrLn("The clause is\n   "++show cl++"\nThe evaluated result is\n   "++show evalClause)
     ; case evalClause of
         xs | any isTruth xs -> prop (cls,assignment)
         [] -> failure ("all terms are AbsurdP "++show cl ++ show assignment)
         [x] -> prop (cls,add x assignment)
         cl2 -> do (cls2,assignment2) <- prop(cls,assignment)
                   return(cl2:cls2,assignment2)
     }                  

saturate (cls,assignment) =
  do ans@(_,as) <- prop (cls,assignment)
     if length as > length assignment 
        then saturate ans
        else return ans

-- Boolean Constraint Propogation   
bcp cls assignment = saturate (cls,assignment)   



backTrack ([],assign) [] = return assign
backTrack (cls,assign) [] = error ("something is wrong "++show cls++show assign)
backTrack (cls,assign) (v:vs) = 
   try (do pair <- saturate(cls,(v,TruthP):assign)
           backTrack pair (adjust vs pair))
       (do pair <- saturate(cls,(v,AbsurdP):assign)
           backTrack pair (adjust vs pair))

-- saturation adds new var assignments, so we need to remove 
-- them from the list of variables that need assignments.
adjust vs (cls,assign) = vs \\ (map fst assign)

sat2 t = 
   do pair <- saturate (t,[]) -- Unit propogation
      backTrack pair (adjust (varsOf t) pair)
  where varsOf [] = []
        varsOf (cl:cls) = foldr acc (varsOf cls) cl
        acc (LetterP x) xs | elem x xs = xs | True = x:xs
        acc (NotP (LetterP x)) xs | elem x xs = xs | True = x : xs
        acc _ xs = xs
    
t2 =  [ [n a,b], [n a,c], [n b,n c,d] ]  
t3 =  [ [n a,b], [n b,n c,d], [n a,c] ]
t4 =  [ [a,b], [n c], [n a,c] ]
t5 = [[a],[c,b],[n a]]

class Monad m => Back m where
  try:: m a -> m a -> m a
  failure:: String -> m a

instance Back Maybe where
  try Nothing comp = comp
  try (Just x) comp = Just x  
  failure s = Nothing

  
instance Back IO where
  failure s = error s
  try command bad =
     do possible <- Control.Exception.try command
        case possible of
          Right ans -> return ans
          Left err -> 
             do putStrLn (show(err::(Control.Exception.ErrorCall)))
                bad
       

        