module PropOptWithList where

import Auxfuns
import Prop hiding (alpha,conjugate,two,one,beta)
import qualified MyMap as DM
import qualified Data.Set as Set
import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ(Doc,text,char,int,(<>),(<+>),($+$),render)


data Lit = Pos Int | Neg Int deriving (Eq,Ord,Show)

data Alpha x = A (Set.Set Lit) [Beta x]  

data Beta x  = B (Set.Set Lit) [Alpha x]  


 
data PropF alpha beta
  = TopF
  | BotF
  | AndF alpha
  | OrF beta
  | LF Lit
 deriving (Eq,Show)

newtype Prop2 = In (PropF (Alpha Prop2) (Beta Prop2))

flatB :: Beta x -> [[PropF y (Beta x)]]
flatB (B is xs) = map (\ x -> [LF x]) (Set.toList is) ++ map f xs
  where f (A is xs) = (map (\ x -> LF x) (Set.toList is)) ++ map OrF xs

oneEach :: [[a]] -> [[a]]
oneEach [] = [[]]
oneEach (x:xs)  = [ y : ys | y <- x, ys <- oneEach xs]


testq (In (OrF b)) = In (foldr alpha TopF (map (foldr beta BotF) ws))
  where zs = flatB b
        ws = oneEach zs


--------------------------------------------------
conjugate (Pos x) (Neg y) | x==y = True
conjugate (Neg x) (Pos y) | x==y = True
conjugate _ _ = False

mapA f (A s bs) = A s (map (mapB f) bs)
mapB f (B s as) = B s (map (mapA f) as)


tt1 = (9 + 10 + (1 * 2 * 3) + (5 * 4) + (7 * (8 + (6 * 12)))) :: Prop Int

-- Apply the distribute Or over And one level
distrib1 (In (OrF (B is xs))) = undefined
distrib1 x = x


--------------------------------------------------------
-- 
trans:: Prop Int -> Prop2
trans x =
  case discrim x of
    TopF -> In TopF
    BotF -> In BotF
    LF x -> In (LF x)
    AndF a -> In(AndF (mapA trans a))
    OrF b -> In(OrF (mapB trans b))

discrim:: Prop Int -> PropF (Alpha (Prop Int)) (Beta(Prop Int))
discrim TruthP = TopF
discrim AbsurdP = BotF
discrim (AndP x y) = alpha (discrim x) (discrim y)
discrim (OrP x y) = beta (discrim x) (discrim y)
discrim (ImpliesP x y) = beta (discrim (notP x)) (discrim y)
discrim (LetterP s) = LF (Pos s)
discrim (NotP (LetterP s)) = LF (Neg s)
discrim (NotP TruthP) = BotF
discrim (NotP AbsurdP) = TopF
discrim (NotP (NotP x)) = discrim x
discrim (NotP (AndP x y)) = beta (discrim (notP x)) (discrim (notP y))
discrim (NotP (OrP x y)) = alpha (discrim (notP x)) (discrim (notP y))
discrim (NotP (ImpliesP x y)) = alpha (discrim x) (discrim (notP y))

one x = Set.insert x Set.empty
two x y = Set.insert x (Set.insert y Set.empty)
add x set = Set.insert x set

{-
beta :: PropF (Alpha (Prop Int)) (Beta (Prop Int)) -> 
        PropF (Alpha (Prop Int)) (Beta (Prop Int)) -> 
        PropF (Alpha (Prop Int)) (Beta (Prop Int))
-}        
beta TopF x = TopF
beta x TopF = TopF
beta BotF x = x
beta x BotF = x
beta (LF x) (LF y) 
  | x==y = LF x
  | conjugate x y = TopF
  | otherwise = OrF (B(two x y) [])
beta (LF x) (OrF (B set xs)) = OrF (B (add x set) xs)
beta (OrF (B set xs)) (LF x) = OrF (B (add x set) xs)
beta (LF x) (AndF z) = OrF (B (one x) [z])
beta (AndF z) (LF x) = OrF (B (one x) [z])
beta (OrF (B s1 xs)) (OrF (B s2 ys)) = OrF (B (Set.union s1 s2) (xs++ys))
beta (OrF (B s xs)) (AndF z) = OrF (B s (z: xs))
beta (AndF z) (OrF (B s xs)) = OrF (B s (z: xs))
beta (AndF z) (AndF w) = OrF (B Set.empty [z,w])

{-   
alpha :: PropF (Alpha (Prop Int)) (Beta (Prop Int)) -> 
         PropF (Alpha (Prop Int)) (Beta (Prop Int)) -> 
         PropF (Alpha (Prop Int)) (Beta (Prop Int))
-}
alpha TopF x = x
alpha x TopF = x
alpha BotF x = BotF
alpha x BotF = BotF
alpha (LF x) (LF y) 
  | x==y = LF x
  | conjugate x y = BotF
  | otherwise = AndF (A(two x y) [])
alpha (LF x) (AndF(A set xs)) = AndF (A (add x set) xs)
alpha (AndF(A set xs)) (LF x) = AndF (A (add x set) xs)
alpha (LF x) (OrF z) = AndF (A (one x) [z])
alpha (OrF z) (LF x) = AndF (A (one x) [z])
alpha (AndF (A s1 xs)) (AndF (A s2 ys)) = AndF (A (Set.union s1 s2) (xs++ys))
alpha (AndF (A s xs)) (OrF z) = AndF (A s (z: xs))
alpha (OrF z) (AndF (A s xs)) = AndF (A s (z: xs))
alpha (OrF z) (OrF w) = AndF (A Set.empty [z,w])
 
-------------------------------------------------
-- Pretty Printing

ppLit (Pos x) = text(show x)
ppLit (Neg x) = text("-"++show x)

ppSet s zs | Set.null s && null zs = PP.empty
ppSet s zs = (PP.fsep(PP.punctuate (text ",") (map ppLit (Set.toList s)))) <> 
             (if (null zs) then PP.empty else text ",")

docsA (A s zs) = (map ppLit (Set.toList s),map ppB zs)
docsB (B s zs) = (map ppLit (Set.toList s),map ppA zs)

sepBy comma xs = PP.fcat(PP.punctuate (text comma) xs)

ppA a = 
  case docsA a of
    ([],xs) -> text "And" <> PP.brackets (sepBy "," xs)
    (is,[]) -> text "And" <> PP.brackets (sepBy "," is)
    (is,xs) -> text "And" <> PP.brackets (PP.sep[sepBy "," is,text ",",sepBy "," xs])
  
ppB b = 
  case docsB b of
    ([],xs) -> text "Or" <> PP.brackets (sepBy "," xs)
    (is,[]) -> text "Or" <> PP.brackets (sepBy "," is)
    (is,xs) -> text "Or" <> PP.brackets (PP.sep[sepBy "," is,text ",",sepBy "," xs])
  
ppPropF (In TopF) = text "T"
ppPropF (In BotF) = text "F"
ppPropF (In (LF x)) = ppLit x
ppPropF (In (AndF x)) = ppA x
ppPropF (In (OrF x)) = ppB x
  

instance Show Prop2 where
  show x = render(ppPropF x)
