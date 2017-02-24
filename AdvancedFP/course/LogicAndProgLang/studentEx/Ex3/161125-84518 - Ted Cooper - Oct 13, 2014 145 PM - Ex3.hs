{-# LANGUAGE StandaloneDeriving #-}
module Ex3 where

import Control.Monad (liftM, liftM2)
import Data.Function (on)
import Data.List (permutations, sort, transpose, union)
import Data.Maybe (fromJust)
import qualified Data.Set as Set
import Debug.Trace (trace)
import Test.QuickCheck hiding(Prop(..))
import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ(Doc,text,char,int,(<>),(<+>),($+$),render)

import SimpleProp hiding(Name(..))


simplify:: Eq n => Prop n -> Prop n
simplify x = undefined

-- almost looks like a functor but it's not
bottomUp :: (Prop n -> Prop n) -> Prop n -> Prop n
bottomUp f (AndP     p q) = f $ (AndP     `on` bottomUp f) p q
bottomUp f (OrP      p q) = f $ (OrP      `on` bottomUp f) p q
bottomUp f (ImpliesP p q) = f $ (ImpliesP `on` bottomUp f) p q
bottomUp f (NotP     p)   = f $ (NotP     .    bottomUp f) p
bottomUp f x              = f x

topDown :: (Prop n -> Prop n) -> Prop n -> Prop n
topDown f x = helper $ f x
  where td                    = topDown f
        helper (AndP     p q) = (AndP     `on` td) p q
        helper (OrP      p q) = (OrP      `on` td) p q
        helper (ImpliesP p q) = (ImpliesP `on` td) p q
        helper (NotP     p)   = (NotP     .    td) p
        helper x              = f x

-- simple algebraic shrinking laws
double :: Eq n => Prop n -> Prop n
double   (AndP     p       q      ) | p == q = p
double   (OrP      p       q      ) | p == q = p
double   (ImpliesP p       q      ) | p == q = TruthP
double   p                                   = p

identity :: Prop n -> Prop n
identity (AndP     TruthP  q      ) = q
identity (AndP     p       TruthP ) = p
identity (AndP     AbsurdP q      ) = AbsurdP
identity (AndP     p       AbsurdP) = AbsurdP
identity (OrP      TruthP  q      ) = TruthP
identity (OrP      p       TruthP ) = TruthP
identity (OrP      AbsurdP q      ) = q
identity (OrP      p       AbsurdP) = p
identity (ImpliesP TruthP  q      ) = q
identity (ImpliesP p       TruthP ) = TruthP
identity (ImpliesP AbsurdP q      ) = TruthP
identity (ImpliesP p       AbsurdP) = NotP p
identity (NotP TruthP)              = AbsurdP
identity (NotP AbsurdP)             = TruthP
identity (NotP (NotP p))            = p
identity p                          = p

deMorgan (NotP (AndP p q))          = (OrP  `on` NotP) p q
deMorgan (NotP (OrP  p q))          = (AndP `on` NotP) p q
deMorgan p                          = p

simp1 :: Eq n => Prop n -> Prop n
simp1 = converge (==) ((topDown $ deMorgan . double . identity))

simp2 :: Eq n => Prop n -> Prop n
simp2 = converge (==) (cnf6 . (topDown $ deMorgan . double . identity))

-- utility functions

converge      :: (a -> a -> Bool) -> (a -> a) -> a -> a 
converge p f   = binBreakFirst p . iterate f

binBreakFirst :: (a -> a -> Bool) -> [a] -> a
binBreakFirst p (x:ys@(y:_))
  | p x y     = y
  | otherwise = binBreakFirst p ys

-- your code to get it into CNF

------------------------------------------------------
-- Elegant method using discriminators
-- the discrim function will be reusable elsewhere.

data Discrim a = Alpha a a | Beta a a | Lit a deriving (Show, Eq, Ord)

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


processCl :: [Prop a] -> [[Prop a]]
processCl [] = [[]]
processCl (p : ps) =
  case (discrim p) of
    Lit x -> map (x:) (processCl ps)
    Alpha x y -> processCl (x : ps) ++ processCl (y : ps)
    Beta x y -> processCl (x : y : ps)
    
cnf5 x = processCl [x]  

cnf6 x = foldr AndP TruthP (map (foldr OrP AbsurdP) (cnf5 x))

-- exercise 3

deriving instance Ord n => Ord (Prop n)

data Result = Taut | Sat | Unsat deriving (Show, Eq, Ord)

infixl 7 |^|

Taut  |^| Taut  = Taut
Taut  |^| Sat   = Taut
Taut  |^| Unsat = Sat
Sat   |^| Taut  = Taut
Sat   |^| Sat   = Sat
Sat   |^| Unsat = Sat
x     |^| y     = y |^| x


tableauxTaut p = tableaux (map simp1 [NotP p]) [Set.empty] == False

tableaux []    paths = --trace ("todos: []" ++ ", paths: " ++ show paths) $
                       let ps = prune paths
                       in --trace ("ps: " ++ show ps) $
                          not . null $ ps
tableaux todos paths = --trace ("todos: " ++ show todos ++ ", paths: " ++ show paths) $
  let (td,tds) = chooseTodo todos
      d        = discrim td
      ps       = prune paths
  in if null ps 
        then False
        else case d of Lit   x   -> let x' = simp1 x in tableaux tds             (map (Set.insert x')   ps)
                       Alpha x y -> let x' = simp1 x
                                        y' = simp1 y in tableaux (tds ++ [x', y']) (map ( Set.insert x' 
                                                                                        . Set.insert y' ) ps)
                       Beta  x y -> let x' = simp1 x
                                        y' = simp1 y in tableaux (tds ++ [x'])    (map (Set.insert x')   ps) 
                                                     || tableaux (tds ++ [y'])    (map (Set.insert y')   ps)

chooseTodo (td:tds) = (td, tds)
chooseTodo []       = error "nothing to do"

prune []     = []
prune (p:ps) = if Set.fold (\pr foundConj -> foundConj || hasConj p pr) False p
                      then prune ps 
                      else p : prune ps

hasConj _    AbsurdP = True  -- if there's an AbsurdP in the path, the path is always false so we can prune it
hasConj path pred    = any ((==) (simp1 (NotP pred))) $ Set.elems path

checkEx3 p = tableauxTaut p == taut p

-- stuff from exercise 1

addvars:: Ord a => Prop a -> [a] -> [a] 
addvars (LetterP x)    = union [x]
addvars (AndP x y)     = addvars x . addvars y
addvars (OrP x y)      = addvars x . addvars y
addvars (ImpliesP x y) = addvars x . addvars y
addvars (NotP x)       = addvars x
addvars _              = id

vars:: Ord a => Prop a -> [a] 
vars = flip addvars []

assigns:: [Int] -> [[(Int,Bool)]]
assigns is = map (zip is) $ strings [True, False] is

strings':: [a] -> [b] -> [[a]]
strings' alphabet is = take (n ^ m) . transpose . take m . scanl f (cycle alphabet) $ is
  where n           = length alphabet
        m           = length is 
        f acc r     = slow acc
        slow (x:xs) = replicate n x ++ slow xs

strings:: [a] -> [b] -> [[a]]
strings alphabet (i:is) = [ (x:xs) | x  <- alphabet
                                   , xs <- strings alphabet is ]
strings alphabet []     = [[]]


vfs:: Prop Int -> [Int -> Bool]
vfs = map ((.) fromJust . flip lookup) . assigns . vars

taut:: Prop Int -> Bool
taut p = all (flip value p) . vfs $ p

equal' :: Prop Int -> Prop Int -> Bool
equal' p q = let vfs'  = vfs p
                 fvs p = map (flip value p) $ vfs'
            in sort (vars p) == sort (vars q) && fvs p == fvs q

equal :: Prop Int -> Prop Int -> Bool
equal p q = taut $ (OrP (AndP p q) (AndP (NotP p) (NotP q)))

-- testing

data Name = A | B | C | D | E deriving (Eq, Ord, Show)

instance Arbitrary a => Arbitrary (Prop a) where
  -- coarbitrary = undefined
  arbitrary = oneof [ return AbsurdP, return TruthP
                    , liftM LetterP arbitrary
                    , liftM NotP arbitrary
                    , liftM2 AndP arbitrary arbitrary
                    , liftM2 OrP arbitrary arbitrary
                    ]

instance Arbitrary Name where
  -- coarbitrary = undefined
  arbitrary = oneof [return A, return B, return C, return D, return E]

instance PPLetter Name where
  ppLetter A = text "A"
  ppLetter B = text "B"
  ppLetter C = text "C"
  ppLetter D = text "D"
  ppLetter E = text "E"


check1 x = equal x (simp1 x)


